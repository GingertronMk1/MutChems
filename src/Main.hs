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
    . reverse
    . take 3
    . sortBy (flip orderOptions)
    . map playerTeamToOption
    . lineupToPlayerTeams
    $ filteredSquad
