-- |
-- Module: Main
module Main (main) where

import Data.Calculated
import Data.Squad
import Data.Time.Clock.System
import Functions.Application
import Functions.Display
import Types.ProspectiveChange
import Types.Variation

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = do
  start <- getSystemTime
  let allProspectiveSquads = addProspectivesInTurn prospectiveAdditions squadNoProspectives
  let squadFilterThreshold' = div squadFilterThreshold (length allProspectiveSquads)
  putStrLn $ printf "Limiting to %s options per iteration" [show squadFilterThreshold']
  let bestOfAllSquads = bestOfAllSquadsFn squadFilterThreshold' allProspectiveSquads
  let html =
        (++ ppNumberOfPlayersOnEveryTeam squadNoProspectives)
          . (++ "\n\n---\n\n")
          . genHtml
          $ bestOfAllSquads
  writeFile "output.md" html
  end <- getSystemTime
  putStrLn $ printf "Done in %s seconds" [show (systemSeconds end - systemSeconds start)]
