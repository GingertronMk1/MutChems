-- |
-- Module: Main
module Main (main) where

import Data.List
import Functions.Application
import System.Environment
import Types.ArgumentList
import Types.BuildObject
import Types.DisplayObject
import Types.InitObject
import Types.Lineup
import Types.Player

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = do
  args <- getArgs
  argumentList <- compileArgumentListAndPrintResults args
  initObject <- decodeJSONInitObject . argInputFile $ argumentList
  processedInitObject <- stepInitObject argumentList initObject
  genHTML processedInitObject argumentList
  putStrLn "Done"

genHTML :: JSONInitObject -> ArgumentList -> IO ()
genHTML
  ( JSONInitObject
      { groupedLineup = gl,
        prospectiveChanges = pcs
      }
    )
  ( ArgumentList
      { argFilterThreshold = filterThreshold,
        argDisregardTeams = disregardTeams,
        argOutputFile = outputFile
      }
    ) = do
    let buildObjects =
          iterativelyApplyProspectiveChanges pcs
            . flattenGroupedLineup
            $ gl
    let displayObjects =
          map (buildObjectToDisplayObject filterThreshold . filterOutTeams disregardTeams) buildObjects
    let html =
          wrapInTag "table"
            . wrapInTag "tr"
            $ printDisplayObjectsAsHtmlTable displayObjects
    let markdownTables =
          printPlayersBelongingToTeamsToMarkdown
            . buildObjectLineup
            . head
            $ buildObjects
    writeFile outputFile $ html ++ "\n" ++ markdownTables
