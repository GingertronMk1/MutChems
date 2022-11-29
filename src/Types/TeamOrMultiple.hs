-- | Module: Types.TeamOrMultiple
module Types.TeamOrMultiple where

import           Data.Bifunctor
import           Data.List
import           Data.Maybe
import           Data.Other
import           Data.Teams
import           Functions.Application
import           Types.Basic

-- * The Main Event.

-- | Options for one or more Teams.
data TeamOrMultiple
  -- | Null value.
  = NoTeam
  -- | A single Team.
  | Team Team
  -- | A single Team with a multiplier, e.g. Raiders x3.
  | MultipleTeam Team Int
  -- | Multiple Teams, e.g. Broncos + Seahawks.
  | Teams [TeamOrMultiple]
  deriving (Eq, Show)

-- | The Ord instance - compare the "lowest" team name in each.
instance Ord TeamOrMultiple where
  -- We're going to base this on MultipleTeams
  -- Comparing 2 MultipleTeams
  compare (MultipleTeam t1 i1)  (MultipleTeam t2 i2) = case compare t1 t2 of
    EQ -> compare i1 i2   -- If it's the same Team then compare how many it is
    c  -> c               -- Otherwise compare the Team
  compare t1@(MultipleTeam _ _) (Team t2)             = compare t1 (MultipleTeam t2 1)
  compare t1@(MultipleTeam _ _) (Teams t2s)           = compare t1 (maximum t2s)
  compare (Team t1)             (Team t2)             = compare (MultipleTeam t1 1) (MultipleTeam t2 1)
  compare (Team t1)             t2@(MultipleTeam _ _) = compare (MultipleTeam t1 1) t2
  compare (Team t1)             (Teams t2s)           = compare (MultipleTeam t1 1) (maximum t2s)
  compare (Teams t1s)           t2@(Team _)           = compare (maximum t1s) t2
  compare (Teams t1s)           t2@(MultipleTeam _ _) = compare (maximum t1s) t2
  compare (Teams t1s)           (Teams t2s)           = compare (maximum t1s) (maximum t2s)
  compare NoTeam                _                     = LT
  compare _                     NoTeam                = GT

-- * Subsequent types that don't deserve their own file

-- | A player and all of their teams.
type PlayerTeams = (Player, [TeamOrMultiple])

-- | A full lineup.
type Lineup = [PlayerTeams]

-- * Functions that take only something of a type defined in this file as argument

-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis.
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam             = []
expandTeamOrMultiple (Team t)           = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts)         = concatMap expandTeamOrMultiple ts

-- | How many options do we get from a given `Lineup`?.
numberOfOptionsFn :: Lineup -> Int
numberOfOptionsFn = product . map (length . snd)

-- | Give me a list of all t`Type.Team` in a given Lineup.
allTeamsFn :: Lineup -> [Team]
allTeamsFn = concatMap expandTeamOrMultiple . concatMap snd

-- | Filter a given squad such that it contains only `squadFilterThreshold` options
filteredSquadFn :: Lineup -> Lineup
filteredSquadFn = filteredSquadFn' 0

-- | Helper for the above - does the actual filtering
filteredSquadFn' ::
  -- | The threshold number - if there are fewer than this many instances of a
  -- t`Type.Team` in a Lineup we can disregard it
  Int ->
  -- | The initial lineup to be filtered
  Lineup ->
  -- | The resultant lineup
  Lineup
filteredSquadFn' threshold s =
  let allTeams = allTeamsFn s
      newS                = map (second . filteredSquadFn'' $ filterFn threshold allTeams) s
      numberOfNewSOptions = numberOfOptionsFn newS
   in if 0 < numberOfNewSOptions && numberOfNewSOptions <= squadFilterThreshold
      then newS
      else filteredSquadFn' (threshold + 1) newS

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
    NoTeam             -> False
    (Team t)           -> filterFn' t
    (MultipleTeam t _) -> filterFn' t
    (Teams teams)      -> any (filterFn threshold ts) teams
    where filterFn' t = numberOfOneTeam t > threshold || t == all32Teams
          numberOfOneTeam t = length . filter (== t) $ ts

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

-- | Change all players with all 32 teams to contain all useful teams
-- useful here being "all other actual teams" - there's no point giving him
-- the option for Jacksonville if nobody else has ever played for them
convertAll32Teams :: Lineup -> Lineup
convertAll32Teams l = map (
                      second
                      . concatMap
                      . convertSingle
                      . rmDups
                      . filter (/= all32Teams)
                      . allTeamsFn
                      $ l
                      ) l

-- | Convert a single TeamOrMultiple to a list of Teams should that TeamOrMultiple
-- be all 32 teams
convertSingle ::
  -- | The list of Teams that should be considered for conversion.
  [Team] ->
  -- | The TeamOrMultiple being converted.
  TeamOrMultiple ->
  -- | The resultant list.
  [TeamOrMultiple]
convertSingle _ NoTeam = [NoTeam]
convertSingle ts team@(Team t) =
  if t == all32Teams
    then map Team ts
    else [team]
convertSingle ts (MultipleTeam t i) =
  if t == all32Teams
    then map (`MultipleTeam` i) ts
    else [MultipleTeam t i]
convertSingle ts (Teams t) = concatMap (convertSingle ts) t

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
compareBasedOnSquad l p1 p2 =
  compare (compareBasedOnSquad' l p1) (compareBasedOnSquad' l p2)

-- | Getting the index for a single player.
compareBasedOnSquad' :: Lineup -> Player -> Int
compareBasedOnSquad' l p = fromMaybe minBound (findIndex ((== p) . fst) l)

-- | Turn a Lineup into one where all of the `Data.Teams.all32Teams` players have been given
-- their teams and filtered by team popularity
convertSquad :: Lineup -> Lineup
convertSquad = filteredSquadFn . convertAll32Teams

-- | Does a given `TeamOrMultiple` contain a given t`Type.Team`.
includesTeam ::
  -- | The t`Type.Team` being searched for.
  Team ->
  -- | The TeamOrMultiple being searched.
  TeamOrMultiple ->
  -- | Does it contain?.
  Bool
includesTeam t = elem t . expandTeamOrMultiple

