{-# LANGUAGE DeriveGeneric #-}

module Types.JSON.Lineup where

import Data.Aeson
import Data.List
import Data.List.Split
import Data.Positions
import Data.Teams
import GHC.Generics
import Text.Printf
import Types.Basic
import Types.ProspectiveChange
import Types.TeamOrMultiple

-- * The JSON Player, base unit of a JSON team

data JSONPlayer = JSONPlayer
  { jpName :: PlayerName,
    jpTeams :: [Team]
  }
  deriving (Eq, Show, Generic)

instance FromJSON JSONPlayer

instance ToJSON JSONPlayer

-- * The JSON Position Group, a collection of JSON Players with a position assigned

data JSONPositionGroup = JSONPositionGroup
  { jpgPosition :: Position,
    jpgPlayers :: [JSONPlayer]
  }
  deriving (Eq, Show, Generic)

instance FromJSON JSONPositionGroup

instance ToJSON JSONPositionGroup

type JSONLineup = [JSONPositionGroup]

-- * The JSON Prospective Change, a JSON representation of a change that could

-- be made
data JSONProspectiveChange
  = JSONAddition JSONPlayer Position
  | JSONReplacement PlayerName JSONPlayer
  | JSONRemovals [PlayerName]
  | JSONNoChange
  deriving (Eq, Show, Generic)

instance FromJSON JSONProspectiveChange

instance ToJSON JSONProspectiveChange

-- * The JSON Init Object, the ultimate JSON representation of the current and

-- future of my lineup
data JSONInitObject = JSONInitObject
  { jsonIOSquad :: JSONLineup,
    jsonIOProspectiveChanges :: [JSONProspectiveChange]
  }
  deriving (Eq, Show, Generic)

instance FromJSON JSONInitObject

instance ToJSON JSONInitObject

-- * Original values --> JSON values

-- | TeamOrMultiple - a straightforward one of encoding as a simple string
teamOrMultipleToJSON :: TeamOrMultiple -> String
teamOrMultipleToJSON NoTeam = ""
teamOrMultipleToJSON (Team t) = t
teamOrMultipleToJSON (MultipleTeam t n) = t ++ "." ++ show n
teamOrMultipleToJSON (Teams ts) = intercalate "|" . map teamOrMultipleToJSON $ ts

-- | A whole lineup
lineupToJSONLineup :: Lineup -> JSONLineup
lineupToJSONLineup = lineupToJSONLineup' []

-- | Helper for the above
lineupToJSONLineup' :: JSONLineup -> Lineup -> JSONLineup
lineupToJSONLineup' jl [] = reverse jl
lineupToJSONLineup' jl (P {pName = currName, pPosition = currPosition, pTeams = currTeams} : ps) =
  let jsonPlayer = JSONPlayer {jpName = currName, jpTeams = map teamOrMultipleToJSON currTeams}
      (currInPosition, others) = partition ((== currPosition) . jpgPosition) jl
   in lineupToJSONLineup'
        ( JSONPositionGroup
            { jpgPlayers = jsonPlayer : concatMap jpgPlayers currInPosition,
              jpgPosition = currPosition
            } :
          others
        )
        ps

-- | ProspectiveChange
prospectiveChangeToJSONProspectiveChange :: ProspectiveChange -> JSONProspectiveChange
prospectiveChangeToJSONProspectiveChange NoChange = JSONNoChange
prospectiveChangeToJSONProspectiveChange (Replacement p1 p2) = JSONReplacement p1 (playerToJSONPlayer p2)
prospectiveChangeToJSONProspectiveChange (Addition newP) = JSONAddition (playerToJSONPlayer newP) (pPosition newP)
prospectiveChangeToJSONProspectiveChange (Removals ps) = JSONRemovals ps

-- | Player
playerToJSONPlayer :: Player -> JSONPlayer
playerToJSONPlayer (P {pName = pn, pTeams = pts}) = JSONPlayer {jpName = pn, jpTeams = map teamOrMultipleToJSON pts}

-- * Transformational functions

-- | Applying a prospective change
applyJSONProspectiveChange :: JSONLineup -> JSONProspectiveChange -> JSONLineup
applyJSONProspectiveChange l JSONNoChange = l
applyJSONProspectiveChange l (JSONReplacement p1 jsonPlayer) =
  case partition (jsonPositionGroupContainsJSONPlayer p1) l of
    ([], _) -> error $ printf "No player called %s exists in lineup"
    (pg : rest, others) ->
      let newPG = removePlayersFromPositionGroup [p1] pg
       in (newPG {jpgPlayers = jsonPlayer : jpgPlayers newPG} : rest) ++ others
applyJSONProspectiveChange l (JSONAddition jsonPlayer position) =
  case partition ((== position) . jpgPosition) l of
    ([], _) -> error $ printf "The position %s does not exist" position
    (pg@(JSONPositionGroup {jpgPlayers = ps}) : rest, others) ->
      (pg {jpgPlayers = jsonPlayer : ps} : rest) ++ others
applyJSONProspectiveChange l (JSONRemovals ps) =
  map (removePlayersFromPositionGroup ps) l

-- | Remove all players with a given name from a position group
-- (There shouldn't be more than one anyway)
removePlayersFromPositionGroup :: [PlayerName] -> JSONPositionGroup -> JSONPositionGroup
removePlayersFromPositionGroup ps pg@(JSONPositionGroup {jpgPlayers = ps2}) =
  pg {jpgPlayers = filter ((`notElem` ps) . jpName) ps2}

-- | Determining whether or not a group contains a player
jsonPositionGroupContainsJSONPlayer :: PlayerName -> JSONPositionGroup -> Bool
jsonPositionGroupContainsJSONPlayer needle =
  elem needle
    . map jpName
    . jpgPlayers

-- | Converting the JSON Init Object to a series of Build Objects
initObjectToBuildObjects :: JSONInitObject -> [BuildObject]
initObjectToBuildObjects
  initObject@(JSONInitObject {jsonIOSquad = squad}) =
    BuildObject
      { buildObjectLineup = jsonLineupToLineup squad,
        buildObjectProspectiveChange = NoChange
      } :
    initObjectToBuildObjects' initObject

-- | Helper for the above, doing all the actual heavy lifting
initObjectToBuildObjects' :: JSONInitObject -> [BuildObject]
initObjectToBuildObjects'
  ( JSONInitObject
      { jsonIOSquad = _,
        jsonIOProspectiveChanges = []
      }
    ) = []
initObjectToBuildObjects'
  ( JSONInitObject
      { jsonIOSquad = squad,
        jsonIOProspectiveChanges = (pc : pcs)
      }
    ) =
    let newSquad = applyJSONProspectiveChange squad pc
     in BuildObject
          { buildObjectLineup = jsonLineupToLineup newSquad,
            buildObjectProspectiveChange = NoChange
          } :
        initObjectToBuildObjects' (JSONInitObject {jsonIOSquad = newSquad, jsonIOProspectiveChanges = pcs})

-- | Converting a JSONPositionGroup to a list of Players
jsonPositionGroupToPlayers :: JSONPositionGroup -> [Player]
jsonPositionGroupToPlayers jpg =
  map
    ( \(JSONPlayer {jpName = _jpName, jpTeams = _jpTeams}) ->
        emptyPlayer
          { pName = _jpName,
            pTeams = convertJSONTeams _jpTeams,
            pPosition = jpgPosition jpg
          }
    )
    (jpgPlayers jpg)

-- | Some special designations for particular players for whom otherwise
-- the JSON file would be interminably long
specialTeamDesignations :: [(String, [TeamOrMultiple])]
specialTeamDesignations =
  [ ("all32Teams", all32Teams),
    ("all32TeamsPlusLegends", all32TeamsPlusLegends),
    ("gronkTeams", teamsForSlots 2 all32TeamsPlusLegends)
  ]

-- | Converting a list of JSON teams to a list of TeamOrMultiples
convertJSONTeams :: [String] -> [TeamOrMultiple]
convertJSONTeams ts =
  case lookup (head ts) specialTeamDesignations of
    Just ts' -> ts'
    Nothing -> map teamToTeamOrMultiple ts

-- | Converting a given Team to a TeamOrMultiple
teamToTeamOrMultiple :: String -> TeamOrMultiple
teamToTeamOrMultiple s
  | '|' `elem` s = Teams $ map teamToTeamOrMultiple . splitOn "|" $ s
  | '.' `elem` s =
    let (teamName, '.' : num) = break (== '.') s
     in MultipleTeam teamName (read num :: Int)
  | otherwise = Team s

-- | Converting a JSONLineup to a regular lineup
jsonLineupToLineup :: JSONLineup -> Lineup
jsonLineupToLineup = concatMap jsonPositionGroupToPlayers

-- | Sorting the Lineup component of a JSONInitObject by position
sortLineupInJSONInitObject :: JSONInitObject -> JSONInitObject
sortLineupInJSONInitObject io@(JSONInitObject {jsonIOSquad = squad}) =
  io {jsonIOSquad = sortJSONLineup squad}

-- | Sorting a JSON Lineup by position
sortJSONLineup :: JSONLineup -> JSONLineup
sortJSONLineup = sortOn sortJSONLineup'

-- | Helper for the above, a "where should this go" function
sortJSONLineup' :: JSONPositionGroup -> Int
sortJSONLineup' (JSONPositionGroup {jpgPosition = pos}) =
  case findIndex ((== pos) . fst) numInPositions of
    Just n -> n
    Nothing -> length numInPositions
