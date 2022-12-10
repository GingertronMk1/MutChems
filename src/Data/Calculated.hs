-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import           Data.Bifunctor
import           Data.List
import           Data.Ord
import           Data.Positions
import           Data.Squad
import           Functions.Application
import           Functions.Display
import           Types.Basic
import           Types.ProspectiveChange
import           Types.TeamOrMultiple
import           Types.Variation

-- | The squad with the team strategy item sorted
processedStrategy :: Lineup
processedStrategy = case strategy of
  NoTeam -> []
  s      -> [("STRATEGY: " ++ ppTeamOrMultiple s, [s], strategyCard)]

-- | Just the base squad and strategy item
squadNoProspectives :: Lineup
squadNoProspectives = filter (\(_,ts,_) -> not (null ts))
                    . (++processedStrategy)
                    . concatMap expandPosition
                    $ baseSquad

-- | The generated list of squads in "chronological" order (or at least planned)
iteratedProspectiveSquads :: [(ProspectiveChange, Lineup)]
iteratedProspectiveSquads = map (second convertSquad)
                          . addProspectivesInTurn prospectiveAdditions
                          $ squadNoProspectives

-- | The final prospective squad
squad :: Lineup
squad = snd . last $ iteratedProspectiveSquads

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
filteredSquad = fst $ filteredSquadFn squad

-- | The number of options available based on the filtered squad.
numberOfOptionsFiltered :: Int
numberOfOptionsFiltered = numberOfOptionsFn filteredSquad

-- | Expanding the above squad such that I can sequence it.
expandedSquad :: [[(Player, TeamOrMultiple, Position)]]
expandedSquad = expandLineup filteredSquad

-- | All variations of chems.
allVariations :: [Variation]
allVariations = map (Variation . sortOn (\(_,ts,_) -> ts)) . sequence $ expandedSquad

-- | Ordered list of Variations.
sortedVariations :: [Variation]
sortedVariations = sort allVariations

-- | Folded variations.
foldedVariations :: [Variation]
foldedVariations = maximumValues sortedVariations

-- | All variations, doubly folded.
doubleFoldedVariations :: [PlayerTeams]
doubleFoldedVariations = doubleFoldVariations foldedVariations

-- | ProspectiveChange, Lineup, and Variations for each ProspectiveChange
bestOfAllSquads :: [(ProspectiveChange, Lineup, Variation)]
bestOfAllSquads = bestOfAllSquadsFn
                . addProspectivesInTurn prospectiveAdditions
                $ squadNoProspectives
