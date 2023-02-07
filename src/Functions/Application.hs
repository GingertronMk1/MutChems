-- |
--
-- Module: Functions.Application
--
-- Application functions, i.e. those which do not care about any types I've created
module Functions.Application where

import Data.List
import Text.Printf

-- | Take the mean of a list of Integral values.
mean :: (Real a) => [a] -> Float
mean ls = fromIntegral (length ls) / realToFrac (sum ls)

-- | Take the minimum distance from a multiple of 5 that a number is.
distanceFrom5 :: Int -> Int
distanceFrom5 = distanceFrom5' . flip mod 5

-- | Take the distance from 5 a given Int value is
distanceFrom5' :: Int -> Int
distanceFrom5' n = min n (5 - n)

-- | Take the average distance from a multiple of 5 that a list of numbers are.
avgDistanceFromMultiplesOf5 :: [Int] -> Float
avgDistanceFromMultiplesOf5 = mean . map distanceFrom5

-- | Ultimately a helper function for orderOptions.
orderListOfInts ::
  -- | First list of Ints.
  [Int] ->
  -- | Second list of Ints.
  [Int] ->
  -- | Tuple containing the Ordering of the lists and a String denoting what has led to this Ordering.
  (Ordering, String)
orderListOfInts xs ys
  | meanComp /= EQ = (meanComp, "Mean")
  | numComp /= EQ = (numComp, "5s")
  | maxComp /= EQ = (maxComp, "Max")
  | distComp /= EQ = (distComp, "Dist")
  | otherwise = (EQ, "Tried all of em")
  where
    meanComp = compare (mean xs) (mean ys)
    num5s = length . filter (0 ==) . map (`mod` 5)
    numComp = compare (num5s xs) (num5s ys)
    distComp = compare (avgDistanceFromMultiplesOf5 ys) (avgDistanceFromMultiplesOf5 xs)
    maxComp = compare (maximum xs) (maximum ys)

-- | Function to get the largest values from a list
maximumValues ::
  Ord a =>
  -- | Input list of `Ord a`s
  [a] ->
  -- | Largest values from that list
  [a]
maximumValues xs = filter (maximum xs <=) xs

-- | Kind of a compression algorithm?
-- Take a list of items and compress them into tuples
-- containing the item and how many times it appears in the list
firstAndLength :: Eq a => [a] -> [(a, Int)]
firstAndLength [] = []
firstAndLength xs@(x : _) =
  let (ins, outs) = partition (x ==) xs
   in (x, length ins) : firstAndLength outs

-- | Convert spaces in a string to non-breaking spaces
unBreakCharacters :: String -> String
unBreakCharacters [] = []
unBreakCharacters (c : cs) =
  let unBrokenCharacters =
        [ (' ', "&nbsp;"),
          ('-', "&#8209;")
        ]
   in case lookup c unBrokenCharacters of
        Just s -> s ++ unBreakCharacters cs
        Nothing -> c : unBreakCharacters cs

-- | Correctly print a list with the appropriate oxford comma
printThingsWithAnd :: [String] -> String
printThingsWithAnd [] = ""
printThingsWithAnd [x] = x
printThingsWithAnd [x, y] = printf "%s and %s" x y
printThingsWithAnd xs = printf "%s, and %s" (intercalate ", " $ init xs) (last xs)

-- | Filtering a list to only those items in the list with more than n instances
filterListByNumber :: Ord a => Int -> [a] -> [a]
filterListByNumber n xs = filter (\x -> length (filter (== x) xs) >= n) xs

-- | Splitting a list into sublists on a predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f = filter (not . null) . splitOn' f

-- | Splitting a list into sublists on a predicate
splitOn' :: (a -> Bool) -> [a] -> [[a]]
splitOn' f s = case dropWhile f s of
  [] -> []
  s' -> w : splitOn' f s''
    where
      (w, s'') = break f s'

-- | Wrap a string in an HTML tag
wrapInTag :: String -> String -> String
wrapInTag input content =
  let (tag : theRest) = words input
      tagAttrs = concatMap (' ' :) theRest
   in printf "<%s%s> %s </%s>" tag tagAttrs content tag

-- | Print an integer with comma separators
ppInteger :: Int -> String
ppInteger = reverse . ppInteger' . reverse . show

-- | Heavy lifting for the above
ppInteger' :: String -> String
ppInteger' str =
  let (taken, theRest) = splitAt 3 str
   in if length taken == 3 && theRest /= ""
        then printf "%s,%s" taken (ppInteger' theRest)
        else taken

-- | Like concatMap but for strings and it puts a newline between them
newLineMap :: (a -> String) -> [a] -> String
newLineMap f = unlines . map f

reverseMap :: [a -> b] -> a -> [b]
reverseMap fs v = map (\f -> f v) fs

splitOnDoubleLines :: String -> [String]
splitOnDoubleLines [] = []
splitOnDoubleLines s = case break (=='\n') s of
  (l, []) -> [l]
  (l, ['\n']) -> [l]
  (l, '\n':s') -> [l] : splitOnDoubleLines s'
  -- (l, s') =

