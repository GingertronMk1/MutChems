module Types.TeamOrMultiple where

import Types.Basic

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

-- | A player and all of their teams.
type PlayerTeams = (Player, [TeamOrMultiple])

-- | A full lineup.
type Lineup = [PlayerTeams]

-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis.
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam             = []
expandTeamOrMultiple (Team t)           = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts)         = concatMap expandTeamOrMultiple ts

