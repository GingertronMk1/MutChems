-- |
-- Module: Main
module Main where

import CalculatedData
import Functions.Display
import Functions.Domain
import Type

-- | The important bit
main :: IO ()
main =
  putStrLn
    . ppOptions
    $ main'

-- | Filtering main''
main' :: [Option]
main' = topOptions main''

-- | A debug function to let me see all options
main'' :: [Option]
main'' = 
  map playerTeamToOption
  . lineupToPlayerTeams
  $ filteredSquad