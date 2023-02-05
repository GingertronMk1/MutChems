-- | Module: Types.ArgumentList
module Types.ArgumentList where

import Data.Calculated
import Data.List
import Functions.Application
import Text.Printf
import Types.Basic

-- | The argument list
data ArgumentList = ArgumentList
  { -- | The list of Teams to disregard
    argDisregardTeams :: [Team],
    -- | The max number of Variations that should be allowed when filtering
    argFilterThreshold :: Int,
    -- | The JSON file to read from
    argInputFile :: String,
    -- | The markdown file to output to
    argOutputFile :: String,
    -- | Should we "increment" the input file
    argStepCount :: Int
  }
  deriving (Show)

-- | The base list of arguments
emptyArgumentList :: ArgumentList
emptyArgumentList =
  ArgumentList
    { argDisregardTeams = [],
      argFilterThreshold = squadFilterThreshold,
      argInputFile = "input.json",
      argOutputFile = "output.md",
      argStepCount = 0
    }

-- | Generate the argument list and show us what they are
compileArgumentListAndPrintResults :: [String] -> IO ArgumentList
compileArgumentListAndPrintResults args = do
  let argumentList@( ArgumentList
                       { argDisregardTeams = disregardTeams,
                         argFilterThreshold = filterThreshold,
                         argInputFile = inputFile,
                         argOutputFile = outputFile,
                         argStepCount = stepCount
                       }
                     ) = argumentsToArgumentList args
  putStrLn $ printf "Taking input from %s" inputFile
  if stepCount > 0
    then putStrLn $ printf "Stepping %s %d times" inputFile stepCount
    else putStrLn "Not stepping"
  putStrLn $ printf "Outputting to %s" outputFile
  case disregardTeams of
    [] -> putStrLn "Not disregarding any teams"
    ts -> putStrLn $ printf "Disregarding %s" (printThingsWithAnd ts)
  putStrLn $ printf "Variation limit: %s" (ppInteger filterThreshold)
  return argumentList

-- | Converting a list of arguments to an ArgumentList
argumentsToArgumentList :: [String] -> ArgumentList
argumentsToArgumentList =
  foldl
    ( \args' s ->
        foldl
          (argumentsToArgumentList' s)
          args'
          processedArgumentPrefixesAndFunctions
    )
    emptyArgumentList

argumentsToArgumentList' ::
  String ->
  ArgumentList ->
  (String, ArgumentList -> String -> ArgumentList) ->
  ArgumentList
argumentsToArgumentList' s args (prefix, f) = maybe args (f args) (stripPrefix prefix s)

processedArgumentPrefixesAndFunctions :: [(String, ArgumentList -> String -> ArgumentList)]
processedArgumentPrefixesAndFunctions =
  map
    (\(s, f) -> ("--" ++ s ++ "=", f))
    argumentPrefixesAndFunctions

argumentPrefixesAndFunctions :: [(String, ArgumentList -> String -> ArgumentList)]
argumentPrefixesAndFunctions =
  [ ("disregardTeams", \args s -> args {argDisregardTeams = splitOn (== ',') s}),
    ("threshold", \args n -> args {argFilterThreshold = read n}),
    ("inputFile", \args f -> args {argInputFile = f}),
    ("outputFile", \args f -> args {argOutputFile = f}),
    ("stepCount", \args n -> args {argStepCount = read n})
  ]
