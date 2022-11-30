-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import           Data.Bifunctor
import           Data.List
import           Data.Ord
import           Data.Other
import           Data.Squad
import           Functions.Application
import           Types.Basic
import           Types.ProspectiveChange
import           Types.TeamOrMultiple
import           Types.Variation
import           Functions.Display

-- | The squad with the team strategy item sorted
processedStrategy :: Lineup
processedStrategy =
  if strategy == NoTeam
  then []
  else [("STRATEGY: " ++ ppTeamOrMultiple strategy, [strategy])]

-- | The constituent parts of a squad combined and sorted by popularity of team.
squad :: Lineup
squad = filter (not . null . snd)
      . addProspectives prospectiveAdditions
      $ baseSquad ++ processedStrategy

filterEachAmount :: Int
filterEachAmount = squadFilterThreshold `div` length prospectiveAdditions + 1

-- | Just the base squad and strategy item
squadNoProspectives :: Lineup
squadNoProspectives = filter (not . null . snd) $ baseSquad ++ processedStrategy

-- | The generated list of squads in "chronological" order (or at least planned)
iteratedProspectiveSquads :: [(ProspectiveChange, Lineup)]
iteratedProspectiveSquads = map (second $ convertSquad filterEachAmount)
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
    . firstAndLength
    $ allTeams

-- | The number of options available based on the above squad.
numberOfOptions :: Int
numberOfOptions = numberOfOptionsFn squad

-- | Filtering the above squad so as to be useful.
filteredSquad :: Lineup
filteredSquad = filteredSquadFn filterEachAmount squad

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
