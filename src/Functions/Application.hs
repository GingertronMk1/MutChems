-- |
--
-- Module: Functions.Application
--
-- Application functions, i.e. those which do not care about any types I've created
module Functions.Application where

import           Data.Char (isDigit)
import           Data.List

-- | Take the mean of a list of Integral values.
mean :: (Real a) => [a] -> Float
mean ls = fromIntegral (length ls) / realToFrac (sum ls)

-- | Take the minimum distance from a multiple of 5 that a number is.
distanceFrom5 :: Int -> Int
distanceFrom5 n = distanceFrom5' $ mod n 5

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
  | sumComp /= EQ = (sumComp, "Sum")
  | numComp /= EQ = (numComp, "5s")
  | maxComp /= EQ = (maxComp, "Max")
  | distComp /= EQ = (distComp, "Dist")
  | otherwise = (EQ, "Tried all of em")
  where
    sumComp = compare (sum xs) (sum ys)
    num5s = length . filter (== 0) . map (`mod` 5)
    numComp = compare (num5s xs) (num5s ys)
    distComp = compare (avgDistanceFromMultiplesOf5 ys) (avgDistanceFromMultiplesOf5 xs)
    maxComp = compare (maximum xs) (maximum ys)

-- | Expand a tuple whos second element is a list into a list of tuples.
expandTuple :: (a, [b]) -> [(a, b)]
expandTuple (a, bs) = [(a, b) | b <- bs]

-- | Expand a list of tuples into a list of lists of tuples
expandList :: [(a, [b])] -> [[(a, b)]]
expandList = map expandTuple

-- | Function to get the largest values from a list
maximumValues ::
  Ord a =>
  -- | Input list of `Ord a`s
  [a] ->
  -- | Largest values from that list
  [a]
maximumValues xs =
  let firstMax = maximum xs
   in filter (firstMax <=) xs

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
  where thisCol = map head ys
        theRest = map tail ys

-- | Kind of a compression algorithm?
-- Take a list of items and compress them into tuples
-- containing the item and how many times it appears in the list
firstAndLength :: Eq a => [a] -> [(a, Int)]
firstAndLength [] = []
firstAndLength xs@(x:_) =
  let (ins, outs) = partition (==x) xs
   in (x, length ins) : firstAndLength outs

-- | Split a list at the first element that satisfies a predicate, removing that element in the process
splitAtPredicate :: (a -> Bool) -> [a] -> ([a], [a])
splitAtPredicate p l = let (befores, _:afters) = break p l in (befores, afters)

-- | Helper/accumulator for the above
splitAtPredicate' :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
splitAtPredicate' _ (l, []) = (reverse l, [])
splitAtPredicate' p (l, x:xs) =
  if p x
  then (reverse l, xs)
  else splitAtPredicate' p (x:l, xs)

-- | Print an integer number with commas as thousands separators
ppNumber :: Integral a => a -> String
ppNumber n =
  let fn = reverse . ppNumber' . reverse
      nString = show $ toInteger n
      (firstBit, lastBit) = break isDigit nString
   in firstBit ++ fn lastBit

-- | Helper function for the above
ppNumber' :: String -> String
ppNumber' n@[_,_,_]  = n
ppNumber' (x:y:z:ns) = (x:y:z:",") ++ ppNumber' ns
ppNumber' ns         = ns

-- | Print a list of Strings finishing with an "and" before the final item
printListWithAnd :: [String] -> String
printListWithAnd [s] = s
printListWithAnd ss@[_,_] = printf "%s and %s" ss
printListWithAnd ss = printf "%s, and %s" [intercalate ", " (init ss), last ss]

-- | Using a function to convert an input to a String, convert a list of such
-- inputs to strings and collate them with newlines
newLineMap :: (a -> String) -> [a] -> String
newLineMap f = intercalate "\n" . map f

-- | Convert spaces in a string to non-breaking spaces
unBreakSpaces :: String -> String
unBreakSpaces = intercalate "&nbsp;" . words
