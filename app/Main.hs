-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Data.Squad
import           Data.Time.Clock.System
import           Functions.Application
import           Functions.Display
import           Types.ProspectiveChange
import           Types.Variation

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  start <- getSystemTime
  let bestOfAllSquads = bestOfAllSquadsFn
                      . addProspectivesInTurn prospectiveAdditions
                      $ squadNoProspectives
  let html = (++ ppNumberOfPlayersOnEveryTeam squadNoProspectives)
           . (++ "\n\n---\n\n")
           . genHtml
           $ bestOfAllSquads
  writeFile "output.md" html
  end <- getSystemTime
  putStrLn $ printf "Done in %s seconds" [show (systemSeconds end - systemSeconds start)]
