-- |
-- Module: Main
module Main (main) where

import Data.Calculated (squadFilterThreshold)
import Types.DisplayObject
import Types.Lineup
import Types.InitObject
import Types.ProspectiveChange
import Types.BuildObject
import Data.List
import Text.Printf

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = do
  genHTML
    "input.json"
    "output.md"
    squadFilterThreshold
  putStrLn "Done"

genHTML :: String -> String -> Int -> IO ()
genHTML inFile outFile n = do
  JSONInitObject
    { groupedLineup = gl,
      prospectiveChanges = pcs
    } <-
    decodeJSONInitObject inFile
  let displayObjects =
        map (buildObjectToDisplayObject n)
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
