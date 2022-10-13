{-|
Module: Main
-}

module Main where

import Functions.Domain
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

