-- |
-- Module: Main
module Main (main) where

import Data.Calculated
import qualified Data.Teams as Teams
import Data.Time.Clock.System
import Functions.Display
import System.Environment
import Text.Printf
import Types.ProspectiveChange
import Types.Variation

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = do
  start <- getSystemTime
  arguments <- getArgs
  let filteredTeam =
        if "--nolegends" `elem` arguments
          then Teams.legends
          else ""
  allProspectiveSquads <- iteratedProspectiveSquads
  let squadNoProspectives = buildObjectLineup . head $ allProspectiveSquads
  let squadFilterThreshold' = div squadFilterThreshold (length allProspectiveSquads)
  if filteredTeam /= ""
    then putStrLn $ "Filtering out " ++ Teams.legends
    else putStrLn "Not filtering"
  putStrLn $ printf "Limiting to %d options per iteration" squadFilterThreshold'
  let bestOfAllSquads = bestOfAllSquadsFn squadFilterThreshold' allProspectiveSquads
  let html =
        (++ ppNumberOfPlayersOnEveryTeam squadNoProspectives)
          . (++ "\n\n---\n\n")
          . genHtml
          $ bestOfAllSquads
  writeFile "output.md" html
  end <- getSystemTime
  putStrLn $ printf "Done in %d seconds" (systemSeconds end - systemSeconds start)
