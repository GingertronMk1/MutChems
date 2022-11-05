-- |
-- Module: Main
module Main where

import           Data.Calculated
import           Functions.Application
import           Functions.Display
import           Functions.Domain
import          Type
import Data.List

-- | Give me the best Variations given a Lineup (if there are no duplicate Players).
main :: IO ()
main =
  let s = squad
  in case duplicatesExist (map fst s) of
    Just (p, n)  -> putStrLn $ printf "There are %s instances of %s. Please add position indicators." [show n, p]
    Nothing -> main' s

-- | Give me the best Variations given a Lineup.
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
  writeFile "output.md" . genMarkdown . doubleFoldVariations $ fv
  putStrLn $ ppVariations fv
