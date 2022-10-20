-- |
--
-- Module: Functions.Application
--
-- Application functions, i.e. those which do not care about any types I've created
module Functions.Application where

import Data.List

-- | Remove duplicate items from a list
rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

-- | Pad a string right with a given char until it is of a certain length
padRight :: Int     -- ^ The length you want the string to be
         -> Char    -- ^ The character you want repeating
         -> String  -- ^ The initial String
         -> String  -- ^ The resultant String
padRight l padding str = str ++ replicate (l - length str) padding

-- | Take the mean of a list of Integral values
mean :: (Integral a) => [a] -> Float
mean ls = fromIntegral (length ls) / fromIntegral (sum ls)

-- | Take the minimum distance from a multiple of 5 that a number is
distanceFrom5 :: Int -> Int
distanceFrom5 n = (\v -> min v (5 - v)) $ mod n 5

-- | Take the average distance from a multiple of 5 that a list of numbers are
avgDistanceFromMultiplesOf5 :: [Int] -> Float
avgDistanceFromMultiplesOf5 = mean . map distanceFrom5

-- | Ultimately a helper function for orderOptions
orderListOfInts :: [Int] -> [Int] -> (Ordering, String)
orderListOfInts xs ys
  | sumComp /= EQ = (sumComp, "Sum")
  | numComp /= EQ = (numComp, "5s")
  | otherwise = (distComp, "Dist")
  where
    sumComp = compare (sum xs) (sum ys)
    num5s = length . filter (== 0) . map (`mod` 5)
    numComp = compare (num5s xs) (num5s ys)
    distComp = compare (avgDistanceFromMultiplesOf5 ys) (avgDistanceFromMultiplesOf5 xs)

-- | A function to convert a string of format "<Data>|<Number>" into a tuple of
-- the described format
breakStringWithNumber :: String -> (String, Int)
breakStringWithNumber t =
  let (t', tn') = break (=='|') t
      tn'' = if tn' == "" then 1 else read (drop 1 tn') :: Int
   in (t', tn'')

-- | Taking a list of strings and replicating them by the number given in `breakStringWithNumber`
-- to make length calculations relatively straightforward
convertSingleToMultiple :: String -> [String]
convertSingleToMultiple t =
  let (t', tn') = breakStringWithNumber t
   in replicate tn' t'

-- | Mapping convertSingleToMultiple over an array of strings and concatenating
-- the result
convertMultiples :: [String] -> [String]
convertMultiples = concatMap convertSingleToMultiple