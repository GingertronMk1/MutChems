-- |
-- Module: Main
module Main where

import CalculatedData
import Functions.Display
import Functions.Domain

-- | The important bit
main :: IO ()
main =
  putStrLn
    . ppOptions
    . reverse
    . topOptions
    . map playerTeamToOption
    . lineupToPlayerTeams
    $ filteredSquad
