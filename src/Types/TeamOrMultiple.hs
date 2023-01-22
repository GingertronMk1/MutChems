-- | Module: Types.TeamOrMultiple
module Types.TeamOrMultiple where

import Data.List
import Data.Maybe
import qualified Data.Positions as P
import Functions.Application
import Text.Printf
import Types.Basic

-- * The Main Event.

-- | The Player object
data Player = P
  { -- | The Player's name
    pName :: PlayerName,
    -- | All of the Player's Teams
    pTeams :: [TeamOrMultiple],
    -- | The Player's position
    pPosition :: Position
  }
  deriving (Eq, Show)

-- | The empty Player, the basis for other players with sensible defaults
emptyPlayer :: Player
emptyPlayer =
  P
    { pName = "",
      pTeams = [],
      pPosition = ""
    }

-- | The position group: a position and the list of Players that play there
data PositionGroup = PositionGroup
  { -- | The position
    pgPosition :: Position,
    -- | The players
    pgPlayers :: [Player]
  }
  deriving (Eq, Show)

-- | The initial lineup - grouped by position
type InitialLineup = [PositionGroup]

-- | The actual Lineup, used for processing
type Lineup = [Player]

-- | Options for one or more Teams.
data TeamOrMultiple
  = -- | Null value.
    NoTeam
  | -- | A single Team.
    Team Team
  | -- | A single Team with a multiplier, e.g. Raiders x3.
    MultipleTeam Team Int
  | -- | Multiple Teams, e.g. Broncos + Seahawks.
    Teams [TeamOrMultiple]
  deriving (Eq, Show)

-- | The Ord instance - compare the "lowest" team name in each.
instance Ord TeamOrMultiple where
  -- We're going to base this on MultipleTeams
  -- Comparing 2 MultipleTeams
  compare (MultipleTeam t1 i1) (MultipleTeam t2 i2) = case compare t1 t2 of
    EQ -> compare i1 i2 -- If it's the same Team then compare how many it is
    c -> c -- Otherwise compare the Team
  compare t1@(MultipleTeam _ _) (Team t2) = compare t1 (MultipleTeam t2 1)
  compare t1@(MultipleTeam _ _) (Teams t2s) = compare t1 (maximum t2s)
  compare (Team t1) (Team t2) = compare (MultipleTeam t1 1) (MultipleTeam t2 1)
  compare (Team t1) t2@(MultipleTeam _ _) = compare (MultipleTeam t1 1) t2
  compare (Team t1) (Teams t2s) = compare (MultipleTeam t1 1) (maximum t2s)
  compare (Teams t1s) t2@(Team _) = compare (maximum t1s) t2
  compare (Teams t1s) t2@(MultipleTeam _ _) = compare (maximum t1s) t2
  compare (Teams t1s) (Teams t2s) = compare (maximum t1s) (maximum t2s)
  compare NoTeam _ = LT
  compare _ NoTeam = GT

-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis.
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam = []
expandTeamOrMultiple (Team t) = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts) = concatMap expandTeamOrMultiple ts

-- | How many options do we get from a given `Lineup`?.
numberOfOptionsFn :: Lineup -> Int
numberOfOptionsFn = product . map (length . pTeams)

-- | Give me a list of all t`Type.Team` in a given Lineup.
allTeamsFn :: Lineup -> [Team]
allTeamsFn = concatMap expandTeamOrMultiple . concatMap pTeams

-- | Filter a given squad such that it contains only `Data.Calculated.squadFilterThreshold` options
filteredSquadFn :: Lineup -> Int -> (Lineup, Int)
filteredSquadFn = filteredSquadFn' 0

-- | Helper for the above - does the actual filtering
filteredSquadFn' ::
  -- | The threshold number - if there are fewer than this many instances of a
  -- t`Type.Team` in a Lineup we can disregard it
  Int ->
  -- | The initial lineup to be filtered
  Lineup ->
  -- | The max number of options
  Int ->
  -- | The resultant lineup
  (Lineup, Int)
filteredSquadFn' threshold s limit
  | numberOfNewSOptions < 0 = nextIfNotZero
  | numberOfNewSOptions == 0 = ([], threshold)
  | numberOfNewSOptions <= limit = (newS, threshold)
  | otherwise = nextIfNotZero
  where
    nextIfNotZero = filteredSquadFn' (threshold + 1) newS limit
    newS = map (filterIndividualPlayer threshold (allTeamsFn s)) s
    numberOfNewSOptions = numberOfOptionsFn newS

filterIndividualPlayer :: Int -> [Team] -> Player -> Player
filterIndividualPlayer threshold teamList player =
  player
    { pTeams =
        filterListOfTeamOrMultiples (filterIndividualTeamOrMultiple threshold teamList)
          . pTeams
          $ player
    }

-- | The function we use to filter the list of `TeamOrMultiple`s in the squad
filterIndividualTeamOrMultiple ::
  -- | The threshold number - if there are fewer than this many instances of a
  -- t`Type.Team` in a Lineup we can disregard it
  Int ->
  -- | The list of t`Type.Team`s we should be comparing against
  [Team] ->
  -- | The `TeamOrMultiple` we're considering
  TeamOrMultiple ->
  -- | The resultant boolean value
  Bool
filterIndividualTeamOrMultiple threshold ts tom = case tom of
  NoTeam -> False
  (Team t) -> filterFn' t
  (MultipleTeam t _) -> filterFn' t
  (Teams teams) -> any (filterIndividualTeamOrMultiple threshold ts) teams
  where
    filterFn' t = numberOfOneTeam t > threshold
    numberOfOneTeam t = length . filter (t ==) $ ts

-- | A helper to be used in the mapping for the above
filterListOfTeamOrMultiples ::
  -- | Nominally the `filterFn` defined in the above's `let` block - should maybe pull that out
  -- into its own function
  (TeamOrMultiple -> Bool) ->
  -- | Input list of TeamOrMultiples
  [TeamOrMultiple] ->
  -- | Resultant list of TeamOrMultiples
  [TeamOrMultiple]
filterListOfTeamOrMultiples f ts = case filter f ts of
  [] -> [NoTeam]
  xs -> xs

-- | Sorting 2 Players based on their position in the initial squad.
compareBasedOnSquad ::
  -- | The initial squad.
  Lineup ->
  -- | The first Player.
  Player ->
  -- | The second Player.
  Player ->
  -- | The resultant Ordering.
  Ordering
compareBasedOnSquad l (P {pName = p1}) (P {pName = p2}) =
  compare (compareBasedOnSquad' l p1) (compareBasedOnSquad' l p2)

-- | Getting the index for a single player.
compareBasedOnSquad' :: Lineup -> PlayerName -> Int
compareBasedOnSquad' l p = fromMaybe minBound (findIndex ((p ==) . pName) l)

-- | Turn a Lineup into one where all of the `Data.Teams.all32Teams` players have been given
-- their teams and filtered by team popularity
convertSquad :: Lineup -> Int -> Lineup
convertSquad n = fst . filteredSquadFn n

-- | See all the players in a Lineup that have a given Team chemistry as an option.
-- Partitions the players into a tuple of the form (ins, outs)
numberOfPlayersOnTeam :: Lineup -> Team -> ([Player], [Player])
numberOfPlayersOnTeam l t = partition (elem t . concatMap expandTeamOrMultiple . pTeams) l

-- | Take a position group and assign its position to all of its constituent players
streamlinePositionGroup :: PositionGroup -> [Player]
streamlinePositionGroup (PositionGroup {pgPosition = positionGroup, pgPlayers = players}) =
  map (\p -> p {pPosition = positionGroup}) players

-- | Streamline the entire lineup
streamlineLineup :: InitialLineup -> Lineup
streamlineLineup = concatMap streamlinePositionGroup

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer.
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam = "-"
ppTeamOrMultiple (Team t) = t
ppTeamOrMultiple (MultipleTeam t i) = printf "%s x%d" t i
ppTeamOrMultiple (Teams ts) = intercalate " | " $ map ppTeamOrMultiple ts

-- * Validity checking a given Lineup

-- | Making sure a lineup is valid for our purposes - no duplicated names
checkLineupIsValid :: Lineup -> Lineup
checkLineupIsValid = checkPlayerNames . checkNumPositions

-- | Ensuring no duplicated names
checkPlayerNames :: Lineup -> Lineup
checkPlayerNames [] = []
checkPlayerNames allPs@(currP@(P {pName = currentPlayerName}) : ps) =
  case filter ((currentPlayerName ==) . pName) allPs of
    [_] -> currP : checkPlayerNames ps
    ps' ->
      error $
        printf
          "There are %d players called %s, in positions %s. This constitutes an invalid lineup."
          (length ps')
          currentPlayerName
          (printThingsWithAnd . map pPosition $ ps')

-- | Ensuring the correct number of players per position
checkNumPositions :: Lineup -> Lineup
checkNumPositions [] = []
checkNumPositions (currP@(P {pPosition = pos}) : ps) = case lookup pos P.numInPositions of
  Just n ->
    let numPlayersInPosition = (+ 1) . length . filter ((== pos) . pPosition) $ ps
     in if n < numPlayersInPosition
          then error $ printf "There are %d players in position %s, more than the maximum of %d" numPlayersInPosition pos n
          else currP : checkNumPositions ps
  Nothing -> error $ printf "There is no provision for position %s" pos

-- | Generate Teams instances for combinations of teams
comboOfTeams :: [[TeamOrMultiple]] -> [TeamOrMultiple]
comboOfTeams = map Teams . sequence

-- | Given a list of TeamOrMultiples, generate all combinations for `n` slots
teamsForSlots :: Int -> [TeamOrMultiple] -> [TeamOrMultiple]
teamsForSlots n = comboOfTeams . replicate n
