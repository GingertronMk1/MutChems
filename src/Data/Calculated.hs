-- |
-- Module: Data.Calculated
module Data.Calculated where

import           Data.Bifunctor
import           Data.List
import           Data.Ord
import           Data.Squad
import           Functions.Application
import           Functions.Domain
import           Type

-- | The constituent parts of a squad combined and sorted by popularity of team.
squad :: Lineup
squad =
  filter (not . null . snd)
    . addProspectives prospectiveAdditions
    $ baseSquad ++ [strategy]

-- | Just the base squad and strategy item
squadNoProspectives :: Lineup
squadNoProspectives = filter (not . null . snd) $ baseSquad ++ [strategy]

-- | The generated list of squads in "chronological" order (or at least planned)
iteratedProspectiveSquads :: [(ProspectiveAddition, Lineup)]
iteratedProspectiveSquads = parallelMap (second convertSquad)
                          . addProspectivesInTurn prospectiveAdditions
                          $ squadNoProspectives

-- | The number of possible variations in each squad in `iteratedProspectiveSquads`
checkSquadNumbers :: [Int]
checkSquadNumbers = map (numberOfOptionsFn . snd) iteratedProspectiveSquads

-- | All teams in the above.
allTeams :: [Team]
allTeams = allTeamsFn squad

-- | All teams but condensed with just the number of each Team.
allTeamsNumbered :: [(Team, Int)]
allTeamsNumbered =
  sortOn (Down . snd)
    . map firstAndLength
    . group
    . sort
    $ allTeams

-- | The number of options available based on the above squad.
numberOfOptions :: Int
numberOfOptions = numberOfOptionsFn squad

-- | Filtering the above squad so as to be useful.
filteredSquad :: Lineup
filteredSquad = filteredSquadFn squad

-- | The number of options available based on the filtered squad.
numberOfOptionsFiltered :: Int
numberOfOptionsFiltered = numberOfOptionsFn filteredSquad

-- | Converting all `Teams.all32` teams in the above squad.
filteredAndConvertedSquad :: Lineup
filteredAndConvertedSquad = convertAll32Teams filteredSquad

-- | Expanding the above squad such that I can sequence it.
expandedSquad :: [[(Player, TeamOrMultiple)]]
expandedSquad = expandList filteredAndConvertedSquad

-- | All variations of chems.
allVariations :: [Variation]
allVariations = map (Variation . sortOn snd) . sequence $ expandedSquad

-- | Ordered list of Variations.
sortedVariations :: [Variation]
sortedVariations = sort allVariations

-- | Folded variations.
foldedVariations :: [Variation]
foldedVariations = maximumValues sortedVariations

-- | All variations, doubly folded.
doubleFoldedVariations :: [PlayerTeams]
doubleFoldedVariations = doubleFoldVariations foldedVariations
