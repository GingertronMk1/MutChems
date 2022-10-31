-- |
-- Module: Type
module Type where

import Data.List
import Functions.Application

-- | Team is shorthand for a String - it is just the name of a team
type Team = String

-- | Player is shorthand for a String - it is just the name of a football player
type Player = String

-- | A player and all of their teams
type PlayerTeams = (Player, [TeamOrMultiple])

-- | A full lineup
type Lineup = [PlayerTeams]

-- | One variation I can have with a Lineup
newtype Variation = Variation [(Player, TeamOrMultiple)] deriving (Eq, Show)

instance Ord Variation where
  compare (Variation v1) (Variation v2) = comparison
    where
      convertFn = map length . group . sort . concatMap (expandTeamOrMultiple . snd)
      expandedV1 = convertFn v1
      expandedV2 = convertFn v2
      (comparison, _) = orderListOfInts expandedV1 expandedV2

-- | A team and a list of all players with that team's chemistry
type TeamPlayer = (Team, [Player])

-- | An option for the whole squad's chemistries
type Option = [TeamPlayer]

-- | Options for one or more Teams
data TeamOrMultiple
  = -- | Null value
    NoTeam
  | -- | A single Team
    Team Team
  | -- | A single Team with a multiplier, e.g. Raiders x3
    MultipleTeam Team Int
  | -- | Multiple Teams, e.g. Broncos + Seahawks
    Teams [TeamOrMultiple]
  deriving (Eq, Show)

-- | The Ord instance - compare the "lowest" team name in each
instance Ord TeamOrMultiple where
  -- All the instances where a single Team is first
  compare (Team t1) (Team t2) = compare t1 t2
  compare (Team t1) (MultipleTeam t2 _) =
    case compare t1 t2 of
      EQ -> LT
      c -> c
  compare t1@(Team _) (Teams t2s) = compare t1 (maximum t2s)
  -- Instances where a MultipleTeam is first
  compare (MultipleTeam t1 i1) (MultipleTeam t2 i2) =
    case compare t1 t2 of
      EQ -> compare i1 i2
      c -> c
  compare t1@(MultipleTeam _ _) (Teams t2s) = compare t1 (maximum t2s)
  -- simple flip of the Team-first instance
  compare t1@(MultipleTeam _ _) t2@(Team _) = compare t2 t1
  -- Instances where a Teams is first
  compare (Teams t1s) (Teams t2s) = compare (maximum t1s) (maximum t2s)
  -- simple flip of the Team-first instance
  compare t1@(Teams _) t2@(Team _) = compare t2 t1
  -- simple flip of the MultipleTeam-first instance
  compare t1@(Teams _) t2@(MultipleTeam _ _) = compare t2 t1
  compare NoTeam _ = LT
  compare _ NoTeam = GT

-- * Helper functions

-- By and large these are just functions that can't live in Functions.Domain,
-- because then using them here would create circular dependencies

-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam = []
expandTeamOrMultiple (Team t) = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts) = concatMap expandTeamOrMultiple ts
