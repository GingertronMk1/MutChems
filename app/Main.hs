-- |
-- Module: Main
module Main (main) where

import Types.Player (genHTML)
import Data.Calculated (squadFilterThreshold)

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = genHTML squadFilterThreshold
