{-|
Module: CalculatedData
-}
module CalculatedData where

import Data
import Type
import Functions

-- | The constituent parts of a squad combined and sorted by popularity of team
squad :: Lineup
squad =
  popularitySort
  $ concatMap
    (concatMap snd)
    [ offense,
      defense,
      specialTeams
    ]

-- | The squad but filtered by popularity
popSquad :: Lineup
popSquad = popFilter squad
