{-# LANGUAGE DeriveGeneric #-}

module Types.ProspectiveChange where

import Data.Aeson
import Functions.Application
import GHC.Generics
import Text.Printf
import Types.Basic
import Types.Lineup
import Types.Player

data ProspectiveChange
  = Addition GroupedPlayer Position
  | Replacement PlayerName GroupedPlayer
  | NoChange
  | Removals [PlayerName]
  deriving (Show, Generic)

instance FromJSON ProspectiveChange

instance ToJSON ProspectiveChange

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

ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (GroupedPlayer {groupedPlayerName = name}) pos) =
  printf "Adding %s at %s" name pos
ppProspectiveChange (Replacement oldName (GroupedPlayer {groupedPlayerName = newName}))
  | oldName == newName = printf "Replacing %s with a different %s" oldName newName
  | otherwise = printf "Replacing %s with %s" oldName newName
ppProspectiveChange (Removals ps) = printf "Removing" $ printThingsWithAnd ps
