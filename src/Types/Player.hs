{-# LANGUAGE DeriveGeneric #-}

module Types.Player where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List (find, group, groupBy, intercalate, maximumBy, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Teams (all32Teams, all32TeamsPlusLegends, legends)
import Functions.Application (firstAndLength, orderListOfInts)
import GHC.Generics (Generic)
import Types.Basic (EncodedTeamOrMultiple, PlayerName, Position, Team)
import Types.TeamOrMultiple (TeamOrMultiple (..), expandTeamOrMultiple, teamsForSlots)

-- * Definitions for the types that go into JSON

data PositionGroup = PositionGroup
  { positionGroupPosition :: Position,
    positionGroupPlayers :: [GroupedPlayer]
  }
  deriving (Eq, Show, Generic)

instance FromJSON PositionGroup

instance ToJSON PositionGroup

data GroupedPlayer = GroupedPlayer
  { groupedPlayerName :: PlayerName,
    groupedPlayerTeams :: [EncodedTeamOrMultiple]
  }
  deriving (Eq, Show, Generic)

instance FromJSON GroupedPlayer

instance ToJSON GroupedPlayer

-- * Definitions for the types that we use for regular analysis

data Player = Player
  { playerName :: PlayerName,
    playerTeams :: [TeamOrMultiple],
    playerPosition :: Position
  }
  deriving (Eq, Ord, Show)

data VariationPlayer = VariationPlayer
  { variationPlayerName :: PlayerName,
    variationPlayerTeam :: TeamOrMultiple,
    variationPlayerPosition :: Position
  }
  deriving (Eq, Ord, Show)

-- * Lineup definitions

newtype GroupedLineup = GroupedLineup [PositionGroup]
  deriving (Eq, Show, Generic)

instance FromJSON GroupedLineup

instance ToJSON GroupedLineup

type FlatLineup = [Player]

-- * Flattening and unflattening lineups

flattenPositionGroup :: PositionGroup -> [Player]
flattenPositionGroup (PositionGroup {positionGroupPosition = pos, positionGroupPlayers = players}) =
  [ Player
      { playerName = pName,
        playerTeams = map decodeTeamOrMultiple pTeams,
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
        map
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
          groups

-- * The Variation

newtype Variation = Variation [VariationPlayer]
  deriving (Eq, Show)

instance Ord Variation where
  compare v1 v2
    | ord1 /= ord2 = compare ord1 ord2
    | n1 /= n2 = compare n1 n2
    | otherwise =
      fst $
        orderListOfInts
          (map snd converted1)
          (map snd converted2)
    where
      converted1 = teamsInVariation v1
      converted2 = teamsInVariation v2
      (ord1, n1) = toNumerical converted1
      (ord2, n2) = toNumerical converted2

-- | Take a Team and how many there are and convert it into an integer so we can
-- more easily compare it to others - ordering them in priority
toNumerical :: [(Team, Int)] -> (Int, Int)
toNumerical cv
  | bestT /= legends && bestN >= 50 = (4, bestN)
  | bestT /= legends && bestN >= 40 = (3, bestN)
  | bestT == legends && bestN >= 40 = (2, bestN)
  | otherwise = (1, bestN)
  where
    (bestT, bestN) = maximumBy (comparing snd) cv

-- | Get a list of all represented teams and how many there are in a given Variation
teamsInVariation :: Variation -> [(Team, Int)]
teamsInVariation =
  firstAndLength
    . variationToTeams

-- | Taking a Variation and reducing it to just the list of Teams it contains
variationToTeams :: Variation -> [Team]
variationToTeams (Variation v) =
  sort
    . concatMap
      ( expandTeamOrMultiple
          . variationPlayerTeam
      )
    $ v

-- | Some special designations for particular players for whom otherwise
-- the JSON file would be interminably long
specialTeamDesignations :: [(EncodedTeamOrMultiple, [TeamOrMultiple])]
specialTeamDesignations =
  [ ("all32Teams", all32Teams),
    ("all32TeamsPlusLegends", all32TeamsPlusLegends),
    ("gronkTeams", teamsForSlots 2 all32TeamsPlusLegends)
  ]

encodeTeamOrMultiples :: [TeamOrMultiple] -> [EncodedTeamOrMultiple]
encodeTeamOrMultiples toms = case find ((== toms) . snd) specialTeamDesignations of
  Just (enc, _) -> [enc]
  Nothing -> map encodeTeamOrMultiple toms

-- | TeamOrMultiple - a straightforward one of encoding as a simple string
encodeTeamOrMultiple :: TeamOrMultiple -> EncodedTeamOrMultiple
encodeTeamOrMultiple NoTeam = ""
encodeTeamOrMultiple (Team t) = t
encodeTeamOrMultiple (MultipleTeam t n) = t ++ "." ++ show n
encodeTeamOrMultiple (Teams ts) = intercalate "|" . map encodeTeamOrMultiple $ ts

decodeTeamOrMultiples :: [EncodedTeamOrMultiple] -> [TeamOrMultiple]
decodeTeamOrMultiples etoms = case find ((== head etoms) . fst) specialTeamDesignations of
  Just (_, toms) -> toms
  Nothing -> map decodeTeamOrMultiple etoms

-- | Converting a given Team to a TeamOrMultiple
decodeTeamOrMultiple :: EncodedTeamOrMultiple -> TeamOrMultiple
decodeTeamOrMultiple s
  | '|' `elem` s = Teams $ map decodeTeamOrMultiple . splitOn "|" $ s
  | '.' `elem` s =
    let (teamName, '.' : num) = break (== '.') s
     in MultipleTeam teamName (read num :: Int)
  | otherwise = Team s

test :: IO ()
test = do
  teamJSON <- BS.readFile "test.json"
  let teams = eitherDecode teamJSON :: Either String GroupedLineup
  case teams of
    Left s -> error s
    Right gl -> test' gl

test' :: GroupedLineup -> IO ()
test' gl = do
  let fl = reduceFlatLineup 10000 . flattenGroupedLineup $ gl
  print fl

-- print . maximum . flatLineupToVariations $ fl

reduceFlatLineup :: Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup = reduceFlatLineup' 0

reduceFlatLineup' :: Int -> Int -> FlatLineup -> (FlatLineup, Int)
reduceFlatLineup' teamThreshold variationLimit lineup
  | numberOfNewLineupOptions < 0 = nextIfNotZero
  | numberOfNewLineupOptions == 1 = (newLineup, teamThreshold)
  | numberOfNewLineupOptions <= variationLimit = (newLineup, teamThreshold)
  | otherwise = nextIfNotZero
  where
    nextIfNotZero = reduceFlatLineup' (teamThreshold + 1) variationLimit newLineup
    newLineup = map (reducePlayerTeams filteredTeams) lineup
    filteredTeams = filterTeamsByNumber teamThreshold allTeamsInLineup
    allTeamsInLineup = concatMap playerTeams lineup
    numberOfNewLineupOptions = product . map (length . playerTeams) $ lineup

reducePlayerTeams :: [TeamOrMultiple] -> Player -> Player
reducePlayerTeams toms p@(Player {playerTeams = ts}) =
  let ts' = case filter (`notElem` toms) ts of
        [] -> [NoTeam]
        ts'' -> ts''
   in p {playerTeams = ts'}

filterTeamsByNumber :: Int -> [TeamOrMultiple] -> [TeamOrMultiple]
filterTeamsByNumber n =
  concat
    . filter (\toms' -> length toms' < n)
    . group
    . sort

flatLineupToVariations :: [Player] -> [Variation]
flatLineupToVariations =
  map Variation
    . mapM playerToVariationPlayers

playerToVariationPlayers :: Player -> [VariationPlayer]
playerToVariationPlayers
  ( Player
      { playerName = pName,
        playerTeams = pTeams,
        playerPosition = pPosition
      }
    ) =
    [ VariationPlayer
        { variationPlayerName = pName,
          variationPlayerTeam = pTeam,
          variationPlayerPosition = pPosition
        }
      | pTeam <- pTeams
    ]
