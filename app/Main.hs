-- |
-- Module: Main
module Main (main) where

import           Functions.Application
import           Types.TeamOrMultiple
import           Data.Calculated
import           Data.List
import           Display
import           Functions.Domain

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  let iterated = iteratedProspectiveSquads
  let prettyPrintedNumberOfOptions = map (ppNumber . numberOfOptionsFn . snd) iterated
  putStrLn $
    printf
      "Calculating best Variations out of %s options (each limited to a maximum of %s)"
      [
        printListWithAnd prettyPrintedNumberOfOptions,
        ppNumber squadFilterThreshold
      ]
  writeFile "output.md"
    . intercalate "\n\n---\n\n"
    . printLineups
    $ iterated
  putStrLn "Done!"
