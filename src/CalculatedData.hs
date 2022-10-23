-- |
-- Module: CalculatedData
module CalculatedData where

import Data
import Functions.Domain
import Type
import Functions.Application

import Data.List
import Data.Ord

-- | The constituent parts of a squad combined and sorted by popularity of team
squad :: Lineup
squad = filter (not . null . snd)
      . concat
      $ [ [strategy], prospectiveAdditions, baseSquad ]

expandedSquad :: [[(Player, TeamOrMultiple)]]
expandedSquad = expandList squad

allTeams :: [(Team, Int)]
allTeams = map (\ts -> (head ts, length ts))
         . sortOn (Down . length)
         . group
         . sort
         . concatMap expandTeamOrMultiple
         . concatMap snd
         $ squad

numberOfOptions :: Int
numberOfOptions = product . map (length . snd) $ squad