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

-- | Using a function to convert an input to a String, convert a list of such
-- inputs to strings and collate them with newlines
newLineMap :: (a -> String) -> [a] -> String
newLineMap f = intercalate "\n" . map f

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

printThingsWithAnd :: [String] -> String
printThingsWithAnd [x] = x
printThingsWithAnd [x, y] = printf "%s and %s" x y
printThingsWithAnd xs = printf "%s, and %s" (intercalate ", " $ init xs) (last xs)

filterListByNumber :: Ord a => Int -> [a] -> [a]
filterListByNumber n =
  concat
    . filter (\toms' -> length toms' >= n)
    . group
    . sort

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f = tail . dropWhile f

splitOn :: (Char -> Bool) -> String -> [String]
splitOn f = filter (not . null) . splitOn' f

splitOn' :: (Char -> Bool) -> String -> [String]
splitOn' f s = case dropWhile f s of
  "" -> []
  s' -> w : splitOn' f s''
    where
      (w, s'') = break f s'
