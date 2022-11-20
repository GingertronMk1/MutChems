-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Data.List
import           Data.Squad
import           Functions.Application
import           Functions.Display

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  putStrLn
    . printf "Generating best lineups for %s possible lineups"
    . (:[])
    . show
    . (+1)
    . length
    $ prospectiveAdditions
  writeFile "output.md"
    . intercalate "\n\n---\n\n"
    . addProspectiveAndPrint prospectiveAdditions
    $ squadNoProspectives
