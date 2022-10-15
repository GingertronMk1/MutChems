-- |
-- Module: Main
module Main where

import CalculatedData
import Functions.Display
import Functions.Domain
import Data.List

-- | The important bit
main :: IO ()
main =
  putStrLn
    . ppOptions
    . sortBy orderOptions
    . map playerTeamToOption
    . lineupToPlayerTeams
    $ filteredSquad
