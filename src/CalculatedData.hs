-- |
-- Module: CalculatedData
module CalculatedData where

import Data
import Functions.Application
import Functions.Domain
import Type

import Data.List
import Data.Ord

-- | The constituent parts of a squad combined and sorted by popularity of team
squad :: Lineup
squad = filter (not . null . snd)
      . concat
      $ [ [strategy], prospectiveAdditions, baseSquad ]


allTeams :: [Team]
allTeams = allTeamsFn squad

allTeamsNumbered :: [(Team, Int)]
allTeamsNumbered = sortOn (Down . snd)
         . map (\ts -> (head ts, length ts))
         . group
         . sort
         $ allTeams

numberOfOptions :: Int
numberOfOptions = numberOfOptionsFn squad

filteredSquad :: Lineup
filteredSquad = filteredSquadFn squad

filteredAndConvertedSquad :: Lineup
filteredAndConvertedSquad = convertAll32Teams filteredSquad

expandedSquad :: [[(Player, TeamOrMultiple)]]
expandedSquad = expandList filteredAndConvertedSquad

allVariations :: [Variation]
allVariations = map (sortOn snd) . sequence $ expandedSquad

sortedVariations :: [Variation]
sortedVariations = sortBy orderVariations allVariations

foldedVariations :: [Variation]
foldedVariations = foldFn orderVariations sortedVariations