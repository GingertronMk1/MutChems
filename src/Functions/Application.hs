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

-- | Expand a tuple whos second element is a list into a list of tuples
expandList :: [(a, [b])] -> [[(a, b)]]
expandList = map (\(a, bs) -> [(a, b) | b <- bs])

-- | The base of the folding function
foldFn :: Ord a => [a] -> [a]
foldFn = foldFn' []

-- | The main folding function
foldFn' :: Ord a => [a]         -- ^ The accumulating list of Orderable elements
        -> [a]                  -- ^ The remaining list being considered
        -> [a]                  -- ^ The result containing the largest items in the initial list
foldFn' [] (x:xs) = foldFn' [x] xs
foldFn' l [] = l
foldFn' (l:ls) (x:xs) = case compare x l of
  GT -> foldFn' [x] xs
  EQ -> foldFn' (x:l:ls) xs
  LT -> foldFn' (l:ls) xs