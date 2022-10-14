{-|

Module: Functions.Application

Application functions, i.e. those which do not care about any types I've created
-}

module Functions.Application where

import Data.List


-- | Remove duplicate items from a list
rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

-- | Pad a string right with a given char until it is of a certain length
padRight :: Int -> Char -> String -> String
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
