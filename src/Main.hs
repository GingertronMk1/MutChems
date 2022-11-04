-- |
-- Module: Main
module Main where

import           Data.Calculated
import           Functions.Application
import           Functions.Display
import           Functions.Domain
import          Type
import Data.List

-- | Give me the best Variations given a Lineup (if there are no duplicate Players)
main :: IO ()
main = do
  let s = squad
  case duplicatesExist (map fst s) of
    Just t  -> putStrLn $ printf "There is a duplicate: multiple instances of `%s`. Add a position indicator and re-run." [t]
    Nothing -> main' s

-- | Give me the best Variations given a Lineup
main' :: Lineup -> IO()
main' s = do
  let fv = foldr foldFn []
         . sort
         . map (Variation . sortOn snd)
         . sequence
         . expandList
         . convertAll32Teams
         . filteredSquadFn
         $ s
      output = ppVariations fv
      doubleFolded = genMarkdown . doubleFoldVariations $ fv
  writeFile "output.md" doubleFolded
  putStrLn output
