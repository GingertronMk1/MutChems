-- |
-- Module: Main
module Main (main) where

import Classes.Data
import Functions.Application
import System.Environment
import Text.Printf
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
  initObject <- readFromFile . argInputFile $ argumentList
  processedInitObject <- stepInitObject argumentList initObject
  genHTML processedInitObject argumentList
  putStrLn "Done"

genHTML :: JSONInitObject -> ArgumentList -> IO ()
genHTML
  initObject
  ( ArgumentList
      { argFilterThreshold = filterThreshold,
        argDisregardTeams = disregardTeams,
        argOutputFile = outputFile
      }
    ) = do
    let buildObjects = initObjectToBuildObjects initObject
    let intermediateObjects = map (buildObjectToIntermediateObject filterThreshold . filterOutTeams disregardTeams) buildObjects
    putStrLn $ printf "Filtered all lineups and converted to a total of %d Variations" (sum . map (length . iObjVariations) $ intermediateObjects)
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

testWrite :: IO ()
testWrite = do
  initObject <- decodeJSONInitObject "input.json"
  print initObject
  writeToFile "output.data" initObject

testRead :: IO ()
testRead = do
  inputData <- readFromFile "output.data" :: IO JSONInitObject
  print inputData
