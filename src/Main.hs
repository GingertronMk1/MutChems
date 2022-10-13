{-|
Module: Main
-}

module Main where

import Domain
import CalculatedData

-- | The important bit
main :: IO ()
main =
  putStrLn
  . ppOptions
  . foldFunction
  . map playerTeamToOption
  . lineupToPlayerTeams
  $ popSquad

