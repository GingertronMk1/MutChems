-- |
-- Module: Main
module Main where

import CalculatedData
import Functions.Domain

-- | The important bit
main :: IO ()
main =
  putStrLn
    . ppOptions
    . foldFunction
    . map playerTeamToOption
    . lineupToPlayerTeams
    $ popSquad
