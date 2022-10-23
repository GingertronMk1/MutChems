-- |
-- Module: Functions.Domain
--
-- Domain functions, i.e. those which are more specific to this project and the
-- data structures it contains
module Functions.Domain where

-- My imports
import qualified Data.Teams as Teams
import Functions.Application
import Type

import Data.Bifunctor
import Data.List

-- Haskell imports

expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple (Team t) = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts) = concatMap expandTeamOrMultiple ts

includesTeam :: Team            -- ^ The Team being searched for
             -> TeamOrMultiple  -- ^ The TeamOrMultiple being searched
             -> Bool            -- ^ Does it contain?
includesTeam t tom = t `elem` expandTeamOrMultiple tom


numberOfOptionsFn :: Lineup -> Int
numberOfOptionsFn = product . map (length . snd)

allTeamsFn :: Lineup -> [Team]
allTeamsFn = concatMap expandTeamOrMultiple
           . concatMap snd


filteredSquadFn :: Lineup -> Lineup
filteredSquadFn s =
  let allTeams = allTeamsFn s
      numberOfOneTeam t = length . filter (==t) $ allTeams
      filterFn' t = numberOfOneTeam t > 3 || t == Teams.all32Teams
      filterFn (Team t) = filterFn' t
      filterFn (MultipleTeam t _) = filterFn' t
      filterFn (Teams ts) = any filterFn ts
   in filter (not . null . snd) . map (Data.Bifunctor.second (filter filterFn)) $ s

convertAll32Teams :: Lineup -> Lineup
convertAll32Teams l =
  let allTeams = rmDups
               . filter (/= Teams.all32Teams)
               . allTeamsFn
               $ l
    in map (second $ concatMap $ convertSingle allTeams) l


convertSingle :: [Team] -> TeamOrMultiple -> [TeamOrMultiple]
convertSingle ts team@(Team t) =
  if t == Teams.all32Teams
  then map Team ts
  else [team]
convertSingle ts (MultipleTeam t i) =
  if t == Teams.all32Teams
  then map (`MultipleTeam` i) ts
  else [MultipleTeam t i]
convertSingle ts (Teams t) = concatMap (convertSingle ts) t

orderVariations :: Variation -> Variation -> Ordering
orderVariations v1 v2 = fst $ orderVariations' v1 v2

orderVariations' :: Variation           -- ^ v1, the first Variation
                 -> Variation           -- ^ v2, the second Variation
                 -> (Ordering, String)  -- ^ the comparison of v1 against v2
orderVariations' v1 v2 =
  let convertFn = map length . group . sort . concatMap (expandTeamOrMultiple . snd)
      expandedV1 = convertFn v1
      expandedV2 = convertFn v2
   in orderListOfInts expandedV1 expandedV2