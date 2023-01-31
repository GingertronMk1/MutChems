module Types.ArgumentList where

import Data.Calculated
import Functions.Application
import Text.Printf
import Types.Basic

data ArgumentList = ArgumentList
  { argDisregardTeams :: [Team],
    argFilterThreshold :: Int,
    argInputFile :: String,
    argOutputFile :: String
  }
  deriving (Show)

emptyArgumentList :: ArgumentList
emptyArgumentList =
  ArgumentList
    { argDisregardTeams = [],
      argFilterThreshold = squadFilterThreshold,
      argInputFile = "input.json",
      argOutputFile = "output.md"
    }

compileArgumentListAndPrintResults :: [String] -> IO ArgumentList
compileArgumentListAndPrintResults args = do
  let argumentList@( ArgumentList
                       { argDisregardTeams = disregardTeams,
                         argFilterThreshold = filterThreshold,
                         argInputFile = inputFile,
                         argOutputFile = outputFile
                       }
                     ) = argumentsToArgumentList args
  putStrLn $ printf "Taking input from %s" inputFile
  putStrLn $ printf "Outputting to %s" outputFile
  case disregardTeams of
    [] -> putStrLn "Not disregarding any teams"
    ts -> putStrLn $ printf "Disregarding %s" (printThingsWithAnd ts)
  putStrLn $ printf "Variation limit: %s" (ppInteger filterThreshold)
  return argumentList

argumentsToArgumentList :: [String] -> ArgumentList
argumentsToArgumentList = argumentsToArgumentList' emptyArgumentList

argumentsToArgumentList' :: ArgumentList -> [String] -> ArgumentList
argumentsToArgumentList' args [] = args
argumentsToArgumentList' args ss =
  foldl argumentsToArgumentList'' args ss

argumentsToArgumentList'' :: ArgumentList -> String -> ArgumentList
argumentsToArgumentList'' args s =
  case break (== '=') s of
    ("--disregardTeams", '=' : ts) ->
      (args {argDisregardTeams = splitOn (== ',') ts})
    ("--threshold", '=' : n) -> (args {argFilterThreshold = read n})
    ("--inputFile", '=' : f) -> (args {argInputFile = f})
    ("--outputFile", '=' : f) -> (args {argOutputFile = f})
    _ -> args
