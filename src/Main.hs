-- |
-- Module: Main
module Main where

import CalculatedData

import Functions.Display

-- | Give me the best Variations given a Lineup
main :: IO ()
main = putStrLn
     . ppVariations
     $ foldedVariations