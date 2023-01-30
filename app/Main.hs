-- |
-- Module: Main
module Main (main) where

import Data.List
import System.Environment
import Text.Printf
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
        intercalate
          "\n"
          [ "<table>",
            "<tr>",
            intercalate "\n" . map (printf "<td>%s</td>" . printDisplayObjectAsHtmlTable) $ displayObjects,
            "</tr>",
            "</table>"
          ]
  writeFile outFile html
