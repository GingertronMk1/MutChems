module Types.Instances where

import           Types.Declarations
import           Data.List
import qualified Data.Teams as T
import           Functions.Application

instance Ord Variation where
  compare (Variation v1) (Variation v2) = case orderListOfInts (map snd $ convertFn v1) (map snd $ convertFn v2) of
    (EQ, _) -> runThroughPreferences [T.legends, T.seahawks, T.eagles] convertedV1 convertedV2
    (c, _) -> c
    where
      convertedV1 = convertFn v1
      convertedV2 = convertFn v2
      convertFn = map (firstAndLength)
                . group
                . sort
                . concatMap (expandTeamOrMultiple . snd)


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





-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis.
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam             = []
expandTeamOrMultiple (Team t)           = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts)         = concatMap expandTeamOrMultiple ts

runThroughPreferences :: [Team] -> [(Team, Int)] -> [(Team, Int)] -> Ordering
runThroughPreferences [] _ _ = EQ
runThroughPreferences (p:ps) v1 v2 = case compare (numTeam v2) (numTeam v1) of
  EQ -> runThroughPreferences ps v1 v2
  c  -> c
  where numTeam tns = case find (\(t,_) -> t == p) tns of
          Just (_,n) -> n
          Nothing    -> 0