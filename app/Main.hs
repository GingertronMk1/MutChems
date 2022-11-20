-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Functions.Display
import           Functions.Domain

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  let fv = map (lineupToVariations . convertSquad) squads
  writeFile "output.md" . intercalation (genMarkdown . doubleFoldVariations) $ fv
  putStrLn . intercalation ppVariations $ fv
