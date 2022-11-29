module Types.Variation where

import Application
import Types.Basic
import Types.TeamOrMultiple
import Data.List
import Data.Teams

-- | One variation I can have with a Lineup.
newtype Variation
  = Variation [(Player, TeamOrMultiple)]
  deriving (Eq, Show)

instance Ord Variation where
  compare (Variation v1) (Variation v2) = case orderListOfInts (map snd $ convertFn v1) (map snd $ convertFn v2) of
    (EQ, _) -> runThroughPreferences [legends, seahawks, eagles] convertedV1 convertedV2
    (c, _) -> c
    where
      convertedV1 = convertFn v1
      convertedV2 = convertFn v2
      convertFn = map firstAndLength
                . group
                . sort
                . concatMap (expandTeamOrMultiple . snd)


runThroughPreferences :: [Team] -> [(Team, Int)] -> [(Team, Int)] -> Ordering
runThroughPreferences [] _ _ = EQ
runThroughPreferences (p:ps) v1 v2 = case compare (numTeam v2) (numTeam v1) of
  EQ -> runThroughPreferences ps v1 v2
  c  -> c
  where numTeam tns = case find (\(t,_) -> t == p) tns of
          Just (_,n) -> n
          Nothing    -> 0
