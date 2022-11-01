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
mean :: (Real a) => [a] -> Float
mean ls = fromIntegral (length ls) / realToFrac (sum ls)

-- | Take the minimum distance from a multiple of 5 that a number is
distanceFrom5 :: Int -> Int
distanceFrom5 n = (\v -> min v (5 - v)) $ mod n 5

-- | Take the average distance from a multiple of 5 that a list of numbers are
avgDistanceFromMultiplesOf5 :: [Int] -> Float
avgDistanceFromMultiplesOf5 = mean . map distanceFrom5

-- | Ultimately a helper function for orderOptions
orderListOfInts :: [Int]              -- ^ First list of Ints
                -> [Int]              -- ^ Second list of Ints
                -> (Ordering, String) -- ^ Tuple containing the Ordering of the lists and a String denoting what has led to this Ordering
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

-- | Expand a tuple whos second element is a list into a list of tuples
expandList :: [(a, [b])] -> [[(a, b)]]
expandList = map (\(a, bs) -> [(a, b) | b <- bs])

-- | A function to plug in to `foldr` to accumulate a list of Variations
foldFn :: Ord a => a  -- ^ The single Variation being compared
       -> [a]         -- ^ The list of Variations we are accumulating
       -> [a]         -- ^ The returned accumulated list of Variations
foldFn new [] = [new]
foldFn new olds@(o:_)
  | new > o = [new]       -- If the new item being compared is greater than the old ones, start the "old" list again with the new item
  | new == o = new:olds   -- If the two are equal, add the new one to the list
  | otherwise = olds      -- Otherwise disregard it and move on

-- | A function to print a formatted string with a series of placeholdser
printf :: String    -- ^ The placeholdered String to be formatted
       -> [String]  -- ^ The list of items to put in those placeholders
       -> String    -- ^ The resultant String
printf = printf' []

printf' :: [String] -- ^ The accumulating list of Strings
        -> String   -- ^ The placeholdered String to be formatted
        -> [String] -- ^ The list of items to put in those placeholders
        -> String   -- ^ The resultant String - a reversed, concatenated version of the accumulator
printf' new "" _ = concat . reverse $ new
printf' new ('%':'s':olds) (item:items) = printf' (item:new) olds items
printf' new (old:olds) items = printf' ([old]:new) olds items