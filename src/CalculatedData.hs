-- |
-- Module: CalculatedData
module CalculatedData where

import Data
import Functions.Domain
import Type

-- | The constituent parts of a squad combined and sorted by popularity of team
squad :: Lineup
squad = filter (not . null . snd)
      . concat
      $ [ [strategy], prospectiveAdditions, offense, defense, specialTeams ]

-- | The squad but not filtered
processedSquad :: Lineup
processedSquad = processSquad squad

-- | The squad but filtered by popularity
filteredSquad :: Lineup
filteredSquad =
  processSquad
    . popFilter
    $ squad

-- | The number of representatives of each team
numOfEachTeam :: [(Team, Int)]
numOfEachTeam = numOfEachTeamFn squad