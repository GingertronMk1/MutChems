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

-- | All teams in the above
allTeams :: [Team]
allTeams = allTeamsFn squad

-- | All teams but condensed with just the number of each Team
allTeamsNumbered :: [(Team, Int)]
allTeamsNumbered = sortOn (Down . snd)
         . map (\ts -> (head ts, length ts))
         . group
         . sort
         $ allTeams

-- | The number of options available based on the above squad
numberOfOptions :: Int
numberOfOptions = numberOfOptionsFn squad

-- | Filtering the above squad so as to be useful
filteredSquad :: Lineup
filteredSquad = filteredSquadFn squad

-- | Converting all `Teams.all32` teams in the above squad
filteredAndConvertedSquad :: Lineup
filteredAndConvertedSquad = convertAll32Teams filteredSquad

-- | Expanding the above squad such that I can sequence it
expandedSquad :: [[(Player, TeamOrMultiple)]]
expandedSquad = expandList filteredAndConvertedSquad

-- | All variations of chems
allVariations :: [Variation]
allVariations = map (sortOn snd) . sequence $ expandedSquad

-- | Ordered list of Variations
sortedVariations :: [Variation]
sortedVariations = sortBy orderVariations allVariations

-- | Folded variations
foldedVariations :: [Variation]
foldedVariations = foldFn orderVariations sortedVariations

doubleFoldedVariations :: [PlayerTeams]
doubleFoldedVariations = doubleFoldVariations foldedVariations