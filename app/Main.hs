-- |
-- Module: Main
module Main (main) where

import Functions.Application
import System.Environment
import Types.ArgumentList
import Types.Basic
import Types.BuildObject
import Types.DisplayObject
import Types.InitObject
import Types.Lineup

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = do
  args <- getArgs
  ArgumentList
    { argDisregardTeams = disregardTeams,
      argFilterThreshold = filterThreshold
    } <-
    compileArgumentListAndPrintResults args

  genHTML
    "input.json"
    "output.md"
    filterThreshold
    disregardTeams
  putStrLn "Done"

genHTML :: String -> String -> Int -> [Team] -> IO ()
genHTML inFile outFile filterThreshold disregardTeams = do
  JSONInitObject
    { groupedLineup = gl,
      prospectiveChanges = pcs
    } <-
    decodeJSONInitObject inFile
  let displayObjects =
        map (buildObjectToDisplayObject filterThreshold . filterOutTeams disregardTeams)
          . iterativelyApplyProspectiveChanges pcs
          . flattenGroupedLineup
          $ gl
  let html =
        wrapInTag "table"
          . wrapInTag "tr"
          $ printDisplayObjectsAsHtmlTable displayObjects
  writeFile outFile html
