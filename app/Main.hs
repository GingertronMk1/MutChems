-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Data.List
import           Data.Squad
import           Functions.Display
import           Types.Variation

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  writeFile "output.md"
    . intercalate "\n\n---\n\n"
    . printLineups
    $ bestOfAllSquadsFn prospectiveAdditions squadNoProspectives
  putStrLn "Done!"
