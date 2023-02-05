-- | Module: Types.TeamOrMultiple
module Types.TeamOrMultiple where

import Data.List
import Text.Printf
import Types.Basic

-- | A TeamOrMultiple - a means of displaying one or more Team Chemistries
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

-- | Options for one or more Teams.
-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis.
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam = []
expandTeamOrMultiple (Team t) = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts) = concatMap expandTeamOrMultiple ts

-- * The Player object - containing a name, a list of TeamOrMultiples, and a position

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer.
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam = "-"
ppTeamOrMultiple (Team t) = t
ppTeamOrMultiple (MultipleTeam t i) = printf "%s x%d" t i
ppTeamOrMultiple (Teams ts) = intercalate " | " $ map ppTeamOrMultiple ts

-- * Validity checking a given Lineup

-- | Generate Teams instances for combinations of teams
comboOfTeams :: [[TeamOrMultiple]] -> [TeamOrMultiple]
comboOfTeams = nub . map comboOfTeams' . sequence

comboOfTeams' :: [TeamOrMultiple] -> TeamOrMultiple
comboOfTeams' toms = case (group . sort) toms of
  [toms'@((Team t) : _)] -> MultipleTeam t $ length toms'
  toms' -> Teams $ concat toms'

-- | Given a list of TeamOrMultiples, generate all combinations for 'n' slots
teamsForSlots :: Int -> [TeamOrMultiple] -> [TeamOrMultiple]
teamsForSlots n = comboOfTeams . replicate n

-- | Converting a TeamOrMultiple its constituent Team
teamOrMultipleToTeams :: TeamOrMultiple -> [Team]
teamOrMultipleToTeams NoTeam = []
teamOrMultipleToTeams (Team t) = [t]
teamOrMultipleToTeams (MultipleTeam t n) = replicate n t
teamOrMultipleToTeams (Teams ts) = concatMap teamOrMultipleToTeams ts

-- | Does a given TeamOrMultiple contain any of a given set of Teams
teamOrMultipleContainsTeams :: [Team] -> TeamOrMultiple -> Bool
teamOrMultipleContainsTeams ts tom =
  let teamOrMultipleTeams = teamOrMultipleToTeams tom
   in not . null $ teamOrMultipleTeams `intersect` ts
