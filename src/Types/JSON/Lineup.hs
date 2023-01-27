{-# LANGUAGE DeriveGeneric #-}

module Types.JSON.Lineup where

import Data.Aeson
import Data.List
import GHC.Generics
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
prospectiveChangeToJSONProspectiveChange (Replacement p1 p) = JSONReplacement p1 (playerToJSONPlayer p)
prospectiveChangeToJSONProspectiveChange (Addition p) = JSONAddition (playerToJSONPlayer p) (pPosition p)
prospectiveChangeToJSONProspectiveChange (Removals ps) = JSONRemovals ps
