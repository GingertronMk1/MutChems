-- |
-- Module: Main
module Main where

import Data.Calculated
import Functions.Domain

import Functions.Display

-- | Give me the best Variations given a Lineup
main :: IO ()
main = do
      let fv = foldedVariations
          output = ppVariations fv
          doubleFolded = genMarkdown . doubleFoldVariations $ fv
      writeFile "output.md" doubleFolded
      putStrLn output