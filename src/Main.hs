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

main' :: [Option]
main' = topOptions main''

main'' :: [Option]
main'' = 
  map playerTeamToOption
  . lineupToPlayerTeams
  $ filteredSquad