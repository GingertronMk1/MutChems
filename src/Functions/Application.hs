-- |
--
-- Module: Functions.Application
--
-- Application functions, i.e. those which do not care about any types I've created
module Functions.Application where

import           Data.List

-- | Remove duplicate items from a list.
rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

-- | Pad a string right with a given char until it is of a certain length.
padRight ::
  -- | The length you want the string to be.
  Int ->
  -- | The character you want repeating.
  Char ->
  -- | The initial String.
  String ->
  -- | The resultant String.
  String
padRight l padding str = str ++ replicate (l - length str) padding

-- | Take the mean of a list of Integral values.
mean :: (Real a) => [a] -> Float
mean ls = fromIntegral (length ls) / realToFrac (sum ls)

-- | Take the minimum distance from a multiple of 5 that a number is.
distanceFrom5 :: Int -> Int
distanceFrom5 n = (\v -> min v (5 - v)) $ mod n 5

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
  let (t', tn') = break (== '|') t
      tn'' = if tn' == "" then 1 else read (drop 1 tn') :: Int
   in (t', tn'')

-- | Expand a tuple whos second element is a list into a list of tuples.
expandList :: [(a, [b])] -> [[(a, b)]]
expandList = map (\(a, bs) -> [(a, b) | b <- bs])

-- | A function to plug in to `foldr` to accumulate a list of Variations.
foldFn ::
  Ord a =>
  -- | The single Variation being compared.
  a ->
  -- | The list of Variations we are accumulating.
  [a] ->
  -- | The returned accumulated list of Variations.
  [a]
foldFn new [] = [new]
foldFn new olds@(o : _) = case compare new o of
  GT -> [new] -- If the new item being compared is greater than the old ones, start the "old" list again with the new item
  EQ -> new : olds -- If the two are equal, add the new one to the list
  LT -> olds -- Otherwise disregard it and move on

-- | A function to print a formatted string with a series of placeholdser.
printf ::
  -- | The placeholdered String to be formatted.
  String ->
  -- | The list of items to put in those placeholders.
  [String] ->
  -- | The resultant String.
  String
printf = printf' []

-- | The printf function that does all the work.
printf' ::
  -- | The accumulating list of Strings.
  [String] ->
  -- | The placeholdered String to be formatted.
  String ->
  -- | The list of items to put in those placeholders.
  [String] ->
  -- | The resultant String - a reversed, concatenated version of the accumulator.
  String
printf' new "" _                              = concat . reverse $ new
printf' new ('%' : 's' : olds) (item : items) = printf' (item : new) olds items
printf' new (old : olds) items                = printf' ([old] : new) olds items

-- | Taking a list, say of Players, and returning the first instance of duplication
-- if there is any, otherwise Nothing.
duplicatesExist :: Eq a => [a] -> Maybe (a, Int)
duplicatesExist [] = Nothing
duplicatesExist (x:xs) = case filter (==x) xs of
  [] -> duplicatesExist xs
  xs' -> Just (x, length xs' + 1)

-- | Rotate a rectangular list.
-- By "rectangular" I mean that each sublist must be of the same length for it
-- to work properly.
rotate :: [[a]] -> [[a]]
rotate = rotate' []

-- | Helper function for the above, dropping and taking.
rotate' :: [[a]] -> [[a]] -> [[a]]
rotate' xs ys
  | null thisCol = reverse xs
  | otherwise = rotate' (thisCol:xs) theRest
  where thisCol = concatMap (take 1) ys
        theRest = map (drop 1) ys