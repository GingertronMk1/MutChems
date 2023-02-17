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

import Types.Printable

-- | Give me the best Variations given a Lineup.
main :: IO ()
main = do
  args <- getArgs
  ArgumentList
    { argFilterThreshold = filterThreshold,
      argDisregardTeams = disregardTeams,
      argOutputFile = outputFile,
      argInputFile = inputFile,
      argStepCount = stepCount
    } <-
    compileArgumentListAndPrintResults args
  initObject <- openAndStepInitObject inputFile stepCount
  let buildObjects = initObjectToBuildObjects initObject
  let intermediateObjects =
        map
          ( buildObjectToIntermediateObject filterThreshold
              . filterOutTeams disregardTeams
          )
          buildObjects
  putStrLn $
    printf
      "Filtered all lineups and converted to a total of %s Variations"
      (length . concatMap iObjVariations $ intermediateObjects)
  let displayObjects = map intermediateObjectToDisplayObject intermediateObjects
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
  putStrLn "Done!"
