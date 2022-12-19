-- | Module: Types.Variation
module Types.Variation where

import           Data.List
import           Data.Ord
import           Data.Teams
import           Functions.Application
import           Types.Basic
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

data VariationObject = VariationObject [VariationPlayer] deriving (Eq, Show)

instance Ord VariationObject where
  compare v1 v2 =
    let converted1 = teamsInVariation v1
        converted2 = teamsInVariation v2
    in case compare (toNumerical converted1) (toNumerical converted2) of
      EQ -> fst $ orderListOfInts (map snd converted1) (map snd converted2)
      nonEQ -> nonEQ

-- | Get a list of all represented teams and how many there are in a given VariationObject
teamsInVariation :: VariationObject -> [(Team, Int)]
teamsInVariation = firstAndLength
                 . variationToTeams
              
-- | Take a Team and how many there are and convert it into an integer so we can
-- more easily compare it to others - ordering them in priority
toNumerical :: [(Team, Int)] -> Int
toNumerical cv
  | bestT /= legends && bestN >= 50 = 4
  | bestT /= legends && bestN >= 40 = 3
  | bestT == legends && bestN >= 40 = 2
  | otherwise                       = 1
  where (bestT, bestN) = maximumBy (comparing snd) cv

-- | Taking a VariationObject and reducing it to just the list of Teams it contains
variationToTeams :: VariationObject -> [Team]
variationToTeams (VariationObject v) = sort . concatMap (\VP {vpTeam = t} -> expandTeamOrMultiple t) $ v

-- | Take a LineupObject and convert it to a list of all Variations
lineupToVariations :: LineupObject -> [VariationObject]
lineupToVariations = map VariationObject
                   . mapM (\P {name = pl, teams = ts, position = pos} -> [VP {vpName = pl, vpTeam = t, vpPosition = pos} | t <- ts])
                   . convertSquad

-- | Convert a LineupObject to its best VariationObject according to the `compare` function
-- defined above
lineupToBestVariation :: LineupObject -> VariationObject
lineupToBestVariation = maximum . lineupToVariations

-- | Generate the best Variations for a set of Lineups and add to the tuples
bestOfAllSquadsFn :: [(ProspectiveChange, LineupObject)] -> [(ProspectiveChange, LineupObject, VariationObject)]
bestOfAllSquadsFn = map bestOfOneSquadFn

-- | Generate the best VariationObject for a given LineupObject and add it to the provided Tuple
bestOfOneSquadFn :: (ProspectiveChange, LineupObject) -> (ProspectiveChange, LineupObject, VariationObject)
bestOfOneSquadFn (c, l) = (c, l, lineupToBestVariation l)

-- | Using the totals of each team in each VariationObject, kind of unfolding them?.
totalsPerSquad :: [VariationPlayer] -> [(Team, Int)]
totalsPerSquad = sortOn (Down . snd)
               . firstAndLength
               . concatMap (\VP {vpTeam = t} -> expandTeamOrMultiple t)

