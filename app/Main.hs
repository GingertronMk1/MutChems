-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Data.List
import           Data.Squad
import           Functions.Display

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  putStrLn $ "Generating best lineups for " ++ show (length prospectiveAdditions + 1) ++ " possible squads"
  writeFile "output.md" . intercalate "\n\n---\n\n" . addProspectiveAndPrint prospectiveAdditions $ squad
