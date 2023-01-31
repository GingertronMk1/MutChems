{-# LANGUAGE DeriveGeneric #-}

-- | Module: Types.Lineup
module Types.Lineup where

import Data.Aeson
import Data.List
import Data.Positions
import Data.Teams
import Functions.Application
import GHC.Generics
import Types.Basic
import Types.Player
import Types.PositionGroup
import Types.TeamOrMultiple

-- | A list of position groups
newtype GroupedLineup = GroupedLineup [PositionGroup]
  deriving (Eq, Show, Generic)

instance FromJSON GroupedLineup

instance ToJSON GroupedLineup

-- | A flattened lineup
type FlatLineup = [Player]

-- | Getting all of the TeamOrMultiples represented in a given FlatLineup
allTeamOrMultiplesInLineup :: FlatLineup -> [TeamOrMultiple]
allTeamOrMultiplesInLineup = concatMap playerTeams

-- | Getting all of the Teams represented in a given FlatLineup
allTeamsInLineup :: FlatLineup -> [Team]
allTeamsInLineup = concatMap teamOrMultipleToTeams . allTeamOrMultiplesInLineup

-- * Lineup definitions

-- | Flattening a PositionGroup into a list of regular Players
flattenPositionGroup :: PositionGroup -> [Player]
flattenPositionGroup (PositionGroup {positionGroupPosition = pos, positionGroupPlayers = players}) =
  [ Player
      { playerName = pName,
        playerTeams = decodeTeamOrMultiples pTeams,
        playerPosition = pos
      }
    | ( GroupedPlayer
          { groupedPlayerName = pName,
            groupedPlayerTeams = pTeams
          }
        ) <-
        players
  ]

-- | Flattening a GroupedLineup into a regular Lineup
flattenGroupedLineup :: GroupedLineup -> FlatLineup
flattenGroupedLineup = concatMap flattenPositionGroup . (\(GroupedLineup gl) -> gl)

-- | Take a FlatLineup and group it by Position
groupFlatLineup :: FlatLineup -> GroupedLineup
groupFlatLineup fl =
  let groups =
        groupBy (\p1 p2 -> playerPosition p1 == playerPosition p2)
          . sortOn playerPosition
          $ fl
   in GroupedLineup $
        sortPositionGroups
          . map
            ( \ps ->
                PositionGroup
                  { positionGroupPosition = playerPosition . head $ ps,
                    positionGroupPlayers =
                      [ GroupedPlayer
                          { groupedPlayerName = pn,
                            groupedPlayerTeams = encodeTeamOrMultiples pts
                          }
                        | (Player {playerName = pn, playerTeams = pts}) <- ps
                      ]
                  }
            )
          $ groups

-- | Get a Player's position in a Lineup
playerPositionInInitialLineup :: FlatLineup -> PlayerName -> Int
playerPositionInInitialLineup initialLineup pName =
  case findIndex ((== pName) . playerName) initialLineup of
    Just n -> n
    Nothing -> 1 + length initialLineup

-- | Sort a list of PositionGroups according to their position in the list
sortPositionGroups :: [PositionGroup] -> [PositionGroup]
sortPositionGroups = sortOn (sortPositionGroups' . positionGroupPosition)

-- | Helper for the above
sortPositionGroups' :: Position -> Int
sortPositionGroups' pos = case findIndex ((== pos) . fst) numInPositions of
  Just n -> n
  Nothing -> 1 + length numInPositions

-- * Filtering a Lineup

-- | Sort the output of `reduceFlatLineupRecursive'`
reduceFlatLineupRecursive :: Int -> FlatLineup -> FlatLineup
reduceFlatLineupRecursive n fl =
  sortOn (playerPositionInInitialLineup fl . playerName) $
    reduceFlatLineupRecursive' n fl

-- | Reduce the number of TeamOrMultiples in a FlatLineup,
-- ensuring all Players have at least one TeamOrMultiple assigned to them
reduceFlatLineupRecursive' :: Int -> FlatLineup -> FlatLineup
reduceFlatLineupRecursive' n fl =
  let (reducedLineup, _) = reduceFlatLineup n fl
   in case partition ((== [NoTeam]) . playerTeams) reducedLineup of
        ([], fl') -> fl'
        (noTeams, fl') ->
          (fl' ++)
            . reduceFlatLineupRecursive' n
            . filter (\(Player {playerName = pName}) -> pName `elem` map playerName noTeams)
            $ fl

-- | One round of reducing a FlatLineup
reduceFlatLineup :: Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup = reduceFlatLineup' 0

-- | Effectively limit the number of possible Variations in a FlatLineup to a target
reduceFlatLineup' :: Int -> Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup' teamThreshold variationLimit lineup
  | numberOfNewLineupOptions < 0 = nextIfNotZero
  | numberOfNewLineupOptions == 0 = (newLineup, teamThreshold)
  | numberOfNewLineupOptions <= variationLimit = (newLineup, teamThreshold)
  | otherwise = nextIfNotZero
  where
    nextIfNotZero = reduceFlatLineup' (teamThreshold + 1) variationLimit newLineup
    newLineup = map (filterInTeamsFromPlayer filteredTeams) lineup
    filteredTeams = filterListByNumber teamThreshold allTeamOrMultiplesInCurrentLineup
    allTeamOrMultiplesInCurrentLineup = allTeamsInLineup lineup
    numberOfNewLineupOptions = product . map (length . playerTeams) $ lineup
