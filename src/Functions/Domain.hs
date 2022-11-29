{-# LANGUAGE TupleSections #-}
-- |
-- Module: Type
module Functions.Domain where

import           Functions.Application
import           Data.Bifunctor
import           Data.List
import           Data.Maybe


-- My imports

-- Haskell imports

-- | The maximum total number of `Variation`s allowed across the whole calculation
maxTotalVariations :: Int
maxTotalVariations = 25 * 1000000

-- | The maximum number of `Variation`s allowed per squad
squadFilterThreshold :: Int
squadFilterThreshold = 5000000


-- * Filtering the squad to limit the number of possible options

-- * Converting those players who have the ability to have any team chemistry
-- applied to them


-- * "Double folding" `Type.Variation`s - sort of rotating the 2D list of teams
-- and players such that we can represent it line-by-line, like in a MarkDown
-- file for instance?

-- * Sorting players in resultant `Type.Variation`s

-- * Adding prospective players to a Lineup

-- * Converting a `Type.Lineup` into something we can actually handle
