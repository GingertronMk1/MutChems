{-# LANGUAGE DeriveGeneric #-}

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

newtype GroupedLineup = GroupedLineup [PositionGroup]
  deriving (Eq, Show, Generic)

instance FromJSON GroupedLineup

instance ToJSON GroupedLineup

type FlatLineup = [Player]

reduceFlatLineupRecursive :: Int -> FlatLineup -> FlatLineup
reduceFlatLineupRecursive n fl =
  sortOn (playerPositionInInitialLineup fl . playerName) $
    reduceFlatLineupRecursive' n fl

reduceFlatLineupRecursive' :: Int -> FlatLineup -> FlatLineup
reduceFlatLineupRecursive' n fl =
  let (reducedLineup, _) = reduceFlatLineup n fl
   in case partition ((== [NoTeam]) . playerTeams) reducedLineup of
        ([], fl') -> fl'
        (noTeams, fl') ->
          (fl' ++)
            . reduceFlatLineupRecursive' n
            . filter (\p -> playerName p `elem` map playerName noTeams)
            $ fl

reduceFlatLineup :: Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup = reduceFlatLineup' 0

reduceFlatLineup' :: Int -> Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup' teamThreshold variationLimit lineup
  | numberOfNewLineupOptions < 0 = nextIfNotZero
  | numberOfNewLineupOptions == 0 = (newLineup, teamThreshold)
  | numberOfNewLineupOptions <= variationLimit = (newLineup, teamThreshold)
  | otherwise = nextIfNotZero
  where
    nextIfNotZero = reduceFlatLineup' (teamThreshold + 1) variationLimit newLineup
    newLineup = map (reducePlayerTeams filteredTeams) lineup
    filteredTeams = filterListByNumber teamThreshold allTeamOrMultiplesInCurrentLineup
    allTeamOrMultiplesInCurrentLineup = allTeamsInLineup lineup
    numberOfNewLineupOptions = product . map (length . playerTeams) $ lineup

allTeamOrMultiplesInLineup :: FlatLineup -> [TeamOrMultiple]
allTeamOrMultiplesInLineup = concatMap playerTeams

allTeamsInLineup :: FlatLineup -> [Team]
allTeamsInLineup = concatMap teamOrMultipleToTeams . allTeamOrMultiplesInLineup

-- * Lineup definitions

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

flattenGroupedLineup :: GroupedLineup -> FlatLineup
flattenGroupedLineup = concatMap flattenPositionGroup . (\(GroupedLineup gl) -> gl)

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

playerPositionInInitialLineup :: FlatLineup -> PlayerName -> Int
playerPositionInInitialLineup initialLineup pName =
  case findIndex ((== pName) . playerName) initialLineup of
    Just n -> n
    Nothing -> 1 + length initialLineup

sortPositionGroups :: [PositionGroup] -> [PositionGroup]
sortPositionGroups = sortOn (sortPositionGroups' . positionGroupPosition)

sortPositionGroups' :: Position -> Int
sortPositionGroups' p = case findIndex ((== p) . fst) numInPositions of
  Just n -> n
  Nothing -> 1 + length numInPositions
