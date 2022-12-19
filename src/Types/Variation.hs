-- | Module: Types.Variation
module Types.Variation where

import           Data.List
import           Data.Ord
import           Data.Teams
import           Functions.Application
import           Types.Basic
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | One variation I can have with a Lineup.
newtype Variation
  = Variation [(Player, TeamOrMultiple, Position)]
  deriving (Eq, Show)

instance Ord Variation where
  compare v1 v2 =
    let converted1 = teamsInVariation v1
        converted2 = teamsInVariation v2
    in case compare (toNumerical converted1) (toNumerical converted2) of
      EQ -> fst $ orderListOfInts (map snd converted1) (map snd converted2)
      nonEQ -> nonEQ

teamsInVariation :: Variation -> [(Team, Int)]
teamsInVariation = firstAndLength
                 . variationToTeams
              
toNumerical :: [(Team, Int)] -> Int
toNumerical cv
  | bestT /= legends && bestN >= 50 = 4
  | bestT /= legends && bestN >= 40 = 3
  | bestT == legends && bestN >= 40 = 2
  | otherwise                       = 1
  where (bestT, bestN) = maximumBy (comparing snd) cv

-- | Taking a Variation and reducing it to just the list of Teams it contains
variationToTeams :: Variation -> [Team]
variationToTeams (Variation v) = sort . concatMap (expandTeamOrMultiple . getSecond) $ v

lineupToVariations :: Lineup -> [Variation]
lineupToVariations = map Variation . mapM (\(pl, ts, pos) -> [(pl, t, pos) | t <- ts]) . convertSquad

lineupToBestVariation :: Lineup -> Variation
lineupToBestVariation = maximum . lineupToVariations

-- | Generate the best Variations for a set of Lineups and add to the tuples
bestOfAllSquadsFn :: [(ProspectiveChange, Lineup)] -> [(ProspectiveChange, Lineup, Variation)]
bestOfAllSquadsFn = map bestOfOneSquadFn

-- | Generate the best Variation for a given Lineup and add it to the provided Tuple
bestOfOneSquadFn :: (ProspectiveChange, Lineup) -> (ProspectiveChange, Lineup, Variation)
bestOfOneSquadFn (c, l) = (c, l, lineupToBestVariation l)

-- | Using the totals of each team in each Variation, kind of unfolding them?.
totalsPerSquad :: [(Player, TeamOrMultiple, Position)] -> [(Team, Int)]
totalsPerSquad = sortOn (Down . snd)
               . firstAndLength
               . concatMap (expandTeamOrMultiple . getSecond)

