-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Data.List
import           Data.Squad
import           Functions.Application
import           Functions.Display
import           Types.ProspectiveChange
import           Types.Variation
import      Data.Time.Clock.System

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  start <- getSystemTime
  writeFile "output.md"
    . intercalate "\n\n---\n\n"
    . printLineups
    . bestOfAllSquadsFn
    . addProspectivesInTurn prospectiveAdditions
    $ squadNoProspectives
  end <- getSystemTime
  putStrLn $ printf "Done in %s seconds" [show (systemSeconds end - systemSeconds start)]
