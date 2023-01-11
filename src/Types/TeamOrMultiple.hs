-- | Module: Types.TeamOrMultiple
module Types.TeamOrMultiple where

import Data.List
import Data.Maybe
import Data.Other
import Functions.Application
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
emptyPlayer = P {pName = "", pTeams = [], pPosition = ""}

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

-- | Filter a given squad such that it contains only `squadFilterThreshold` options
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
    allTeams = allTeamsFn s
    newS = map (\p -> p {pTeams = filteredSquadFn'' (filterFn threshold allTeams) . pTeams $ p}) s
    numberOfNewSOptions = numberOfOptionsFn newS

-- | The function we use to filter the list of `TeamOrMultiple`s in the squad
filterFn ::
  -- | The threshold number - if there are fewer than this many instances of a
  -- t`Type.Team` in a Lineup we can disregard it
  Int ->
  -- | The list of t`Type.Team`s we should be comparing against
  [Team] ->
  -- | The `TeamOrMultiple` we're considering
  TeamOrMultiple ->
  -- | The resultant boolean value
  Bool
filterFn threshold ts tom = case tom of
  NoTeam -> False
  (Team t) -> filterFn' t
  (MultipleTeam t _) -> filterFn' t
  (Teams teams) -> any (filterFn threshold ts) teams
  where
    filterFn' t = numberOfOneTeam t > threshold
    numberOfOneTeam t = length . filter (t ==) $ ts

-- | A helper to be used in the mapping for the above
filteredSquadFn'' ::
  -- | Nominally the `filterFn` defined in the above's `let` block - should maybe pull that out
  -- into its own function
  (TeamOrMultiple -> Bool) ->
  -- | Input list of TeamOrMultiples
  [TeamOrMultiple] ->
  -- | Resultant list of TeamOrMultiples
  [TeamOrMultiple]
filteredSquadFn'' f ts = case filter f ts of
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
ppTeamOrMultiple (MultipleTeam t i) = printf "%s x%s" [t, show i]
ppTeamOrMultiple (Teams ts) = intercalate "/" $ map ppTeamOrMultiple ts

-- | Making sure a lineup is valid for our purposes - no duplicated names
checkLineupIsValid :: Lineup -> Lineup
checkLineupIsValid l = checkLineupIsValid' l l

-- | Helper function for the above
checkLineupIsValid' :: Lineup -> Lineup -> Lineup
checkLineupIsValid' [] l = l
checkLineupIsValid' allPs@(P {pName = currentPlayerName} : ps) l =
  case filter ((currentPlayerName ==) . pName) allPs of
    [_] -> checkLineupIsValid' ps l
    ps' ->
      error $
        printf
          "There are %s players called %s, in positions %s. This constitutes an invalid lineup."
          [ show (length ps'),
            currentPlayerName,
            intercalate ", " . map pPosition $ ps'
          ]

-- | Generate Teams instances for combinations of teams
comboOfTeams :: [[TeamOrMultiple]] -> [TeamOrMultiple]
comboOfTeams = map Teams . sequence
