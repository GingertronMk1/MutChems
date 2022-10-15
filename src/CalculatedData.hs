-- |
-- Module: CalculatedData
module CalculatedData where

import Data
import Functions.Domain
import Type

-- | The constituent parts of a squad combined and sorted by popularity of team
squad :: Lineup
squad = (strategy:) . concatMap (concatMap snd) $ [ offense, defense, specialTeams ]

-- | The squad but not filtered
processedSquad :: Lineup
processedSquad = processSquad squad

-- | The squad but filtered by popularity
filteredSquad :: Lineup
filteredSquad =
  processSquad
    . popFilter
    $ squad
