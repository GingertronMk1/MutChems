-- |
-- Module: Main
module Main (main) where

import Data.Calculated (squadFilterThreshold)
import Types.Player (genHTML)

-- | Give me the best Variations given a Lineup.
main :: IO ()
main =
  genHTML
    "input.json"
    "output.md"
    squadFilterThreshold
