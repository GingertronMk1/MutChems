{-# LANGUAGE DeriveGeneric #-}

module Types.Player where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BS
import Data.List (find, group, groupBy, intercalate, intersect, maximumBy, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Teams (all32Teams, all32TeamsPlusLegends, legends)
import Functions.Application (firstAndLength, orderListOfInts)
import GHC.Generics (Generic)
import Text.Printf (printf)
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

data ProspectiveChange
  = Addition GroupedPlayer Position
  | Replacement PlayerName GroupedPlayer
  | NoChange
  | Removals [PlayerName]
  deriving (Eq, Show, Generic)

instance FromJSON ProspectiveChange

instance ToJSON ProspectiveChange

data JSONInitObject = JSONInitObject
  { groupedLineup :: GroupedLineup,
    prospectiveChanges :: [ProspectiveChange]
  }
  deriving (Eq, Show, Generic)

instance FromJSON JSONInitObject

instance ToJSON JSONInitObject

decodeJSONInitObject :: String -> IO JSONInitObject
decodeJSONInitObject s = do
  teamJSON <- BS.readFile s
  case eitherDecode teamJSON of
    Left err -> error err
    Right tj -> return tj

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

data BuildObject = BuildObject
  { buildObjectLineup :: FlatLineup,
    buildObjectProspectiveChange :: ProspectiveChange
  }
  deriving (Eq, Show)

data DisplayObject = DisplayObject
  { displayObjectVariation :: Variation,
    displayObjectProspectiveChange :: ProspectiveChange
  }
  deriving (Eq, Show)

ppDisplayObject :: DisplayObject -> String
ppDisplayObject (DisplayObject {displayObjectVariation = var}) =
  intercalate "\n"
    . map
      ( \(VariationPlayer {variationPlayerName = vpn, variationPlayerTeam = vpt, variationPlayerPosition = vpp}) ->
          printf "%s | %s | %s" vpn (show vpt) vpp
      )
    . variationToList
    $ var

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

groupedPlayerToPlayer :: GroupedPlayer -> Position -> Player
groupedPlayerToPlayer (GroupedPlayer {groupedPlayerName = name, groupedPlayerTeams = teams}) pos =
  Player {playerName = name, playerTeams = decodeTeamOrMultiples teams, playerPosition = pos}

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

variationToList :: Variation -> [VariationPlayer]
variationToList (Variation v) = v

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

reducePlayerTeams ::
  [Team] -> -- The list of banned TeamOrMultiples
  Player -> -- Initial Player
  Player -- Fixed Player
reducePlayerTeams toms p@(Player {playerTeams = ts}) =
  let ts' = case filter (teamOrMultipleContainsTeams toms) ts of
        [] -> [NoTeam]
        ts'' -> ts''
   in p {playerTeams = ts'}

filterListByNumber :: Ord a => Int -> [a] -> [a]
filterListByNumber n =
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

teamOrMultipleToTeams :: TeamOrMultiple -> [Team]
teamOrMultipleToTeams NoTeam = []
teamOrMultipleToTeams (Team t) = [t]
teamOrMultipleToTeams (MultipleTeam t n) = replicate n t
teamOrMultipleToTeams (Teams ts) = concatMap teamOrMultipleToTeams ts

teamOrMultipleContainsTeams :: [Team] -> TeamOrMultiple -> Bool
teamOrMultipleContainsTeams ts tom =
  null $ intersect (teamOrMultipleToTeams tom) ts

-- * Applying prospective changes

applyProspectiveChange :: ProspectiveChange -> FlatLineup -> FlatLineup
applyProspectiveChange NoChange fl = fl
applyProspectiveChange (Addition gp position) fl =
  let (befores, afters) = break ((== position) . playerPosition) fl
   in befores ++ (groupedPlayerToPlayer gp position : afters)
applyProspectiveChange (Replacement oldP newP) fl =
  case break ((== oldP) . playerName) fl of
    (_, []) -> error $ printf "No player called %s" oldP
    (befores, (Player {playerPosition = oldPosition}) : afters) ->
      befores ++ (groupedPlayerToPlayer newP oldPosition : afters)
applyProspectiveChange (Removals ps) fl = filter ((`notElem` ps) . playerName) fl

iterativelyApplyProspectiveChanges ::
  [ProspectiveChange] ->
  FlatLineup ->
  [BuildObject]
iterativelyApplyProspectiveChanges pcs fl =
  BuildObject {buildObjectLineup = fl, buildObjectProspectiveChange = NoChange} : iterativelyApplyProspectiveChanges' pcs fl

iterativelyApplyProspectiveChanges' ::
  [ProspectiveChange] ->
  FlatLineup ->
  [BuildObject]
iterativelyApplyProspectiveChanges' [] _ = []
iterativelyApplyProspectiveChanges' (pc : pcs) fl =
  let newFL = applyProspectiveChange pc fl
   in BuildObject {buildObjectLineup = newFL, buildObjectProspectiveChange = pc} : iterativelyApplyProspectiveChanges' pcs newFL

buildObjectToDisplayObject :: BuildObject -> DisplayObject
buildObjectToDisplayObject (BuildObject {buildObjectLineup = l, buildObjectProspectiveChange = pc}) =
  DisplayObject
    { displayObjectVariation = maximum . flatLineupToVariations . fst . reduceFlatLineup 10000 $ l,
      displayObjectProspectiveChange = pc
    }

-- * Pretty printing things as HTML Tables

ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (GroupedPlayer {groupedPlayerName = name}) pos) =
  printf "Adding %s at %s" name pos
ppProspectiveChange (Replacement oldName (GroupedPlayer {groupedPlayerName = newName}))
  | oldName == newName = printf "Replacing %s with a different %s" oldName newName
  | otherwise = printf "Replacing %s with %s" oldName newName
ppProspectiveChange (Removals ps) = printf "Removing" $ intercalate ", " ps

printDisplayObjectAsHtmlTable :: DisplayObject -> String
printDisplayObjectAsHtmlTable
  ( DisplayObject
      { displayObjectVariation = var,
        displayObjectProspectiveChange = pc
      }
    ) =
    intercalate
      "\n"
      [ "<table>",
        printf "<tr><th colspan=2>%s</th></tr>" $ ppProspectiveChange pc,
        intercalate "\n"
          . map
            ( \(VariationPlayer {variationPlayerName = vpn, variationPlayerTeam = vpt}) ->
                printf "<tr><td>%s</td><td>%s</td></tr>" vpn (show vpt)
            )
          . variationToList
          $ var,
        "</table>"
      ]

-- * IO Actions for testing purposes

test :: IO ()
test = do
  JSONInitObject
    { groupedLineup = gl,
      prospectiveChanges = pcs
    } <-
    decodeJSONInitObject "test.json"
  let displayObjects =
        map buildObjectToDisplayObject
          . iterativelyApplyProspectiveChanges pcs
          . flattenGroupedLineup
          $ gl
  let html =
        intercalate
          "\n"
          [ "<table>",
            "<tr>",
            intercalate "\n" . map (printf "<td>%s</td>" . printDisplayObjectAsHtmlTable) $ displayObjects,
            "</tr>",
            "</table>"
          ]
  writeFile "testout.md" html
