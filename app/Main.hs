-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Data.List
import           Data.Squad
import           Functions.Application
import           Functions.Display
import           Functions.Domain

-- | Give me the best Variations given a Lineup.
main :: IO()
main = do
  let iterated = iteratedProspectiveSquads
  putStrLn $
    "Calculating best Variations out of "
    ++ intercalate ", " . map (show . numberOfOptionsFn . snd) $ iterated 
    ++ " options"
  writeFile "output.md"
    . intercalate "\n\n---\n\n"
    . printLineups
    $ iterated
  putStrLn "Done!"
