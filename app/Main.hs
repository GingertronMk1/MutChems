-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Functions.Display
import           Functions.Domain

-- | Give me the best Variations given a Lineup (if there are no duplicate Players).
main :: IO ()
main = main'

-- | Give me the best Variations given a Lineup.
main' :: IO()
main' = do
  let fv = map (lineupToVariations . convertSquad) squads
  let markdown = intercalation (genMarkdown . doubleFoldVariations) fv
  writeFile "output.md" markdown
  putStrLn . intercalation ppVariations $ fv
