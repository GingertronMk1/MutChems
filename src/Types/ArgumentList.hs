module Types.ArgumentList where

import Data.Calculated
import Functions.Application
import Text.Printf
import Types.Basic

data ArgumentList = ArgumentList
  { argDisregardTeams :: [Team],
    argFilterThreshold :: Int
  }
  deriving (Show)

emptyArgumentList :: ArgumentList
emptyArgumentList =
  ArgumentList
    { argDisregardTeams = [],
      argFilterThreshold = squadFilterThreshold
    }

compileArgumentListAndPrintResults :: [String] -> IO ArgumentList
compileArgumentListAndPrintResults args = do
  let argumentList@( ArgumentList
                       { argDisregardTeams = disregardTeams,
                         argFilterThreshold = filterThreshold
                       }
                     ) = argumentsToArgumentList args
  case disregardTeams of
    [] -> putStrLn "Not disregarding any teams"
    ts -> putStrLn $ printf "Disregarding %s" (printThingsWithAnd ts)
  putStrLn $ printf "Variation limit: %s" (ppInteger filterThreshold)
  return argumentList

argumentsToArgumentList :: [String] -> ArgumentList
argumentsToArgumentList = argumentsToArgumentList' emptyArgumentList

argumentsToArgumentList' :: ArgumentList -> [String] -> ArgumentList
argumentsToArgumentList' args [] = args
argumentsToArgumentList' args (s : ss) = case break (== '=') s of
  ("--disregardTeams", '=' : ts) ->
    argumentsToArgumentList' (args {argDisregardTeams = splitOn (== ',') ts}) ss
  ("--threshold", '=' : n) ->
    argumentsToArgumentList' (args {argFilterThreshold = read n}) ss
  _ -> argumentsToArgumentList' args ss
