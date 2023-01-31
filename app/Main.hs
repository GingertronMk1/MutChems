-- |
-- Module: Main
module Main (main) where

import Functions.Application
import System.Environment
import Types.ArgumentList
import Types.BuildObject
import Types.DisplayObject
import Types.InitObject
import Types.Lineup

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = do
  args <- getArgs
  argumentList <- compileArgumentListAndPrintResults args
  genHTML argumentList
  putStrLn "Done"

genHTML :: ArgumentList -> IO ()
genHTML
  ( ArgumentList
      { argFilterThreshold = filterThreshold,
        argDisregardTeams = disregardTeams,
        argInputFile = inputFile,
        argOutputFile = outputFile
      }
    ) = do
    JSONInitObject
      { groupedLineup = gl,
        prospectiveChanges = pcs
      } <-
      decodeJSONInitObject inputFile
    let displayObjects =
          map (buildObjectToDisplayObject filterThreshold . filterOutTeams disregardTeams)
            . iterativelyApplyProspectiveChanges pcs
            . flattenGroupedLineup
            $ gl
    let html =
          wrapInTag "table"
            . wrapInTag "tr"
            $ printDisplayObjectsAsHtmlTable displayObjects
    writeFile outputFile html
