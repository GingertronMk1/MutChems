{-# LANGUAGE DeriveGeneric #-}

module Types.JSON.Lineup where

import Data.Aeson
import Data.List
import Data.List.Split
import GHC.Generics
import Text.Printf
import Types.Basic
import Types.ProspectiveChange
import Types.TeamOrMultiple

data JSONPlayer = JSONPlayer
  { jpName :: PlayerName,
    jpTeams :: [Team]
  }
  deriving (Eq, Show, Generic)

instance FromJSON JSONPlayer

instance ToJSON JSONPlayer

playerToJSONPlayer :: Player -> JSONPlayer
playerToJSONPlayer (P {pName = pn, pTeams = pts}) = JSONPlayer {jpName = pn, jpTeams = map teamOrMultipleToJSON pts}

data JSONPositionGroup = JSONPositionGroup
  { jpgPosition :: Position,
    jpgPlayers :: [JSONPlayer]
  }
  deriving (Eq, Show, Generic)

instance FromJSON JSONPositionGroup

instance ToJSON JSONPositionGroup

type JSONLineup = [JSONPositionGroup]

teamOrMultipleToJSON :: TeamOrMultiple -> String
teamOrMultipleToJSON NoTeam = ""
teamOrMultipleToJSON (Team t) = t
teamOrMultipleToJSON (MultipleTeam t n) = t ++ "." ++ show n
teamOrMultipleToJSON (Teams ts) = intercalate "|" . map teamOrMultipleToJSON $ ts

lineupToJSONLineup :: Lineup -> JSONLineup
lineupToJSONLineup = lineupToJSONLineup' []

lineupToJSONLineup' :: JSONLineup -> Lineup -> JSONLineup
lineupToJSONLineup' jl [] = jl
lineupToJSONLineup' jl (p@(P {pPosition = currPosition}) : ps) =
  let jsonPlayer = JSONPlayer {jpName = pName p, jpTeams = map teamOrMultipleToJSON (pTeams p)}
   in case partition ((== currPosition) . jpgPosition) jl of
        (currPoses, jls) ->
          lineupToJSONLineup'
            ( JSONPositionGroup
                { jpgPlayers = jsonPlayer : concatMap jpgPlayers currPoses,
                  jpgPosition = currPosition
                } :
              jls
            )
            ps
        ([], jls) ->
          lineupToJSONLineup'
            ( JSONPositionGroup
                { jpgPlayers = [jsonPlayer],
                  jpgPosition = currPosition
                } :
              jls
            )
            ps

data JSONProspectiveChange
  = JSONAddition JSONPlayer Position
  | JSONReplacement PlayerName JSONPlayer
  | JSONRemovals [PlayerName]
  | JSONNoChange
  deriving (Eq, Show, Generic)

instance FromJSON JSONProspectiveChange

instance ToJSON JSONProspectiveChange

data JSONInitObject = JSONInitObject {
  jsonIOSquad :: JSONLineup,
  jsonIOProspectiveChanges :: [JSONProspectiveChange]
}  deriving (Eq, Show, Generic)

instance FromJSON JSONInitObject

instance ToJSON JSONInitObject

prospectiveChangeToJSONProspectiveChange :: ProspectiveChange -> JSONProspectiveChange
prospectiveChangeToJSONProspectiveChange NoChange = JSONNoChange
prospectiveChangeToJSONProspectiveChange (Replacement p1 p) = JSONReplacement p1 (playerToJSONPlayer p)
prospectiveChangeToJSONProspectiveChange (Addition p) = JSONAddition (playerToJSONPlayer p) (pPosition p)
prospectiveChangeToJSONProspectiveChange (Removals ps) = JSONRemovals ps

applyJSONProspectiveChange :: JSONLineup -> JSONProspectiveChange -> JSONLineup
-- Get all the position groups, find the one with the player in it, and replace him with the new one
applyJSONProspectiveChange l (JSONReplacement p1 jsonPlayer) = 
  case partition (jsonPositionGroupContainsJSONPlayer p1) l of
    ([], _) -> error $ printf "No player called %s exists in lineup" 
    (pg@(JSONPositionGroup { jpgPlayers = ps }) : rest, others) ->
      (removePlayersFromPositionGroup [p1] pg : rest) ++ others
applyJSONProspectiveChange l (JSONAddition jsonPlayer position) =
  case partition ((==position) . jpgPosition) l of
    ([], _) -> error $ printf "The position %s does not exist" position
    (pg@(JSONPositionGroup { jpgPlayers = ps}) : rest, others) ->
      (pg {jpgPlayers = jsonPlayer : ps} : rest) ++ others
applyJSONProspectiveChange l (JSONRemovals ps) =
  map (removePlayersFromPositionGroup ps) l

removePlayersFromPositionGroup :: [PlayerName] -> JSONPositionGroup -> JSONPositionGroup
removePlayersFromPositionGroup ps pg@(JSONPositionGroup {jpgPlayers = ps2}) =
  pg { jpgPlayers = filter (\p -> jpName p `notElem` ps) ps2}

jsonPositionGroupContainsJSONPlayer :: PlayerName -> JSONPositionGroup -> Bool
jsonPositionGroupContainsJSONPlayer needle = elem needle . map jpName . jpgPlayers

initObjectToBuildObjects :: JSONInitObject -> [BuildObject]
initObjectToBuildObjects initObject@(JSONInitObject {
    jsonIOSquad = squad,
    jsonIOProspectiveChanges = prospectiveChanges
  }) = BuildObject {
    buildObjectLineup = jsonLineupToLineup squad,
    buildObjectProspectiveChange = NoChange
  } : initObjectToBuildObjects' initObject

initObjectToBuildObjects' :: JSONInitObject -> [BuildObject]
initObjectToBuildObjects' (JSONInitObject {
    jsonIOSquad = _,
    jsonIOProspectiveChanges = []
  }) = []
initObjectToBuildObjects' (JSONInitObject {
    jsonIOSquad = squad,
    jsonIOProspectiveChanges = (pc:pcs)
  }) = let newSquad = applyJSONProspectiveChange squad pc
        in BuildObject {
          buildObjectLineup = jsonLineupToLineup newSquad,
          buildObjectProspectiveChange = NoChange
        } : initObjectToBuildObjects' (JSONInitObject {jsonIOSquad = newSquad, jsonIOProspectiveChanges = pcs})



jsonPositionGroupToPlayers :: JSONPositionGroup -> [Player]
jsonPositionGroupToPlayers jpg =
  map (\p -> emptyPlayer {pName = jpName p, pTeams = map jsonTeamOrMultipleToTeamOrMultiple (jpTeams p), pPosition = jpgPosition jpg}) (jpgPlayers jpg)

jsonTeamOrMultipleToTeamOrMultiple :: String -> TeamOrMultiple
jsonTeamOrMultipleToTeamOrMultiple s
  | '|' `elem` s = Teams $ map jsonTeamOrMultipleToTeamOrMultiple . splitOn "|" $ s
  | '.' `elem` s = let (teamName, '.':num) = break (=='.') s
                    in MultipleTeam teamName (read num :: Int)
  | otherwise = Team s

jsonLineupToLineup :: JSONLineup -> Lineup
jsonLineupToLineup = concatMap jsonPositionGroupToPlayers
