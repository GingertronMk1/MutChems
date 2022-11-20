-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import Data.List
import           Functions.Application
import           Functions.Display
import           Functions.Domain
import           Type

-- | Give me the best Variations given a Lineup (if there are no duplicate Players).
main :: IO ()
main = main'

-- | Give me the best Variations given a Lineup.
main' :: IO()
main' = do
  let fv = map (lineupToVariations . convertSquad) squads
  let markdown = intercalation (genMarkdown . doubleFoldVariations) fv
  putStrLn markdown
  writeFile "output.md" markdown 
  -- putStrLn . intercalation . map ppVariations $ fv
