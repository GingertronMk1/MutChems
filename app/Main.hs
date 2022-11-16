-- |
-- Module: Main
module Main (main) where

import           Data.Calculated
import           Data.List
import           Functions.Application
import           Functions.Display
import           Functions.Domain
import           Type

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
  let fSquad = convertAll32Teams
         . filteredSquadFn
         $ s

  putStrLn $ printf "Iterating over %s possible options...\n\n" [show $ numberOfOptionsFn fSquad]

  let fv = take 10
         . maximumValues
         . sort
         . map (Variation . sortOn snd)
         . sequence
         . expandList
         $ fSquad
  writeFile "output.md" . genMarkdown . doubleFoldVariations $ fv
  putStrLn $ ppVariations fv
