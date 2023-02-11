-- | Module: Types.Lineup
module Types.Lineup where

import Classes.Data
import Data.List
import Data.Ord
import Data.Positions
import Data.Teams
import Functions.Application
import Text.Printf
import Types.Basic
import Types.Player
import Types.PositionGroup
import Types.TeamOrMultiple

-- | A list of position groups
newtype GroupedLineup = GroupedLineup [PositionGroup]
  deriving (Eq, Show)

instance Data GroupedLineup where
  toData (GroupedLineup gl) = intercalate "\n\n" . map toData $ gl
  fromData s =
    let posGroups = filter (not . null) . splitOnDoubleLines $ s
     in GroupedLineup $ map fromData posGroups

-- | A flattened lineup
type FlatLineup = [Player]

-- | Getting all of the TeamOrMultiples represented in a given FlatLineup
allTeamOrMultiplesInLineup :: FlatLineup -> [TeamOrMultiple]
allTeamOrMultiplesInLineup = concatMap playerTeams

-- | Getting all of the Teams represented in a given FlatLineup
allTeamsInLineup :: FlatLineup -> [Team]
allTeamsInLineup = concatMap (nub . teamOrMultipleToTeams) . allTeamOrMultiplesInLineup

-- * Lineup definitions

-- | Flattening a PositionGroup into a list of regular Players
flattenPositionGroup :: PositionGroup -> [Player]
flattenPositionGroup (PositionGroup {positionGroupPosition = pos, positionGroupPlayers = players}) =
  [ Player
      { playerName = pName,
        playerTeams = decodeTeamOrMultiples pTeams,
        playerPosition = readToPositionData pos
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
                  { positionGroupPosition = show . playerPosition . head $ ps,
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
reduceFlatLineupRecursive' threshold fl =
  case partition ((== [NoTeam]) . playerTeams) . fst . reduceFlatLineup threshold $ fl of
    ([], fl') -> fl'
    (noTeams, fl') ->
      (fl' ++)
        . reduceFlatLineupRecursive' (threshold `div` 2)
        . filter (\(Player {playerName = pName}) -> pName `elem` map playerName noTeams)
        $ fl

-- | One round of reducing a FlatLineup
reduceFlatLineup :: Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup = reduceFlatLineup' 0

-- | Effectively limit the number of possible Variations in a FlatLineup to a target
reduceFlatLineup' :: Int -> Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup' teamThreshold variationLimit lineup =
  let newLineup = map (filterInTeamsFromPlayer filteredTeams) lineup
      filteredTeams = filterListByNumber teamThreshold . allTeamsInLineup $ lineup
      numberOfNewLineupOptions = product . map (length . playerTeams) $ lineup
   in if 0 <= numberOfNewLineupOptions
        && numberOfNewLineupOptions <= variationLimit
        then (newLineup, teamThreshold)
        else reduceFlatLineup' (teamThreshold + 1) variationLimit newLineup

-- | Get a list of all the players that do and do not belong to every Team represented in a lineup
printPlayerTeamsInLineup :: FlatLineup -> [(Team, [Player], [Player])]
printPlayerTeamsInLineup fl =
  sortOn (\(_, ps, _) -> Down . length $ ps)
    . map (printPlayersBelongingToTeam fl)
    . getAllTeamsFromLineup
    $ fl

-- | Get lists of players that do and do not belong to a given Team
printPlayersBelongingToTeam :: [Player] -> Team -> (Team, [Player], [Player])
printPlayersBelongingToTeam ps t =
  let (ins, outs) =
        partition
          ( (/= [NoTeam])
              . playerTeams
              . filterInTeamsFromPlayer [t]
          )
          ps
   in ( t,
        ins,
        outs
      )

-- | Print tables consisting of the players that do and do not belong to each Team in a Lineup
printPlayersBelongingToTeamsToMarkdown :: FlatLineup -> String
printPlayersBelongingToTeamsToMarkdown fl =
  let playersInTeams = printPlayerTeamsInLineup fl
   in intercalate "\n\n"
        . map printPlayersAsMarkDownSection
        $ playersInTeams

-- | Print a table consisting of the players that do and do not belong to a given Team
printPlayersAsMarkDownSection :: (Team, [Player], [Player]) -> String
printPlayersAsMarkDownSection (t, ins, outs) =
  intercalate
    "\n"
    $ [ wrapInTag "h2" $ printf "<a id=\"%s\">%s (%d/%d)</a>" t t (length ins) (length (ins ++ outs)),
        "\n",
        wrapInTag "h4" $ printf "Has %s chemistry" t,
        "\n",
        "| Player | Position |",
        "|:---|---|"
      ]
      ++ map printPlayerAsMarkDownRow ins
      ++ [ "\n",
           wrapInTag "h4" $ printf "Does not have %s chemistry" t,
           "\n",
           "| Player | Position |",
           "|:---|---|"
         ]
      ++ map printPlayerAsMarkDownRow outs

-- | Print a player and their position as a markdown table row
printPlayerAsMarkDownRow :: Player -> String
printPlayerAsMarkDownRow (Player {playerName = pName, playerPosition = pPosition}) =
  printf "| %s | %s |" (unBreakCharacters pName) (unBreakCharacters . show $ pPosition)

-- | Get a list of all teams represented in a lineup
getAllTeamsFromLineup :: FlatLineup -> [Team]
getAllTeamsFromLineup = nub . concatMap getAllTeamsFromPlayer

-- | Get a list of all teams to which a player belongs
getAllTeamsFromPlayer :: Player -> [Team]
getAllTeamsFromPlayer = nub . concatMap expandTeamOrMultiple . playerTeams
