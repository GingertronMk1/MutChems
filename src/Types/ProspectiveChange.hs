{-# LANGUAGE DeriveGeneric #-}

-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import Data.Aeson
import Functions.Application
import GHC.Generics
import Text.Printf
import Types.Basic
import Types.Lineup
import Types.Player

-- | A Prospective Change to a FlatLineup
data ProspectiveChange
  = -- | An addition to the lineup
    Addition GroupedPlayer Position
  | -- | Replacing a given Player with another one
    Replacement PlayerName GroupedPlayer
  | -- | Remove players
    Removals [PlayerName]
  | -- | No change
    NoChange
  deriving (Show, Generic)

instance FromJSON ProspectiveChange

instance ToJSON ProspectiveChange

-- | Apply a given prospective change
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

-- | Nicely print a given prospective change
ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (GroupedPlayer {groupedPlayerName = name}) pos) =
  printf "Adding %s at %s" (unBreakCharacters name) pos
ppProspectiveChange (Replacement oldName (GroupedPlayer {groupedPlayerName = newName}))
  | oldName == newName = printf "Replacing %s with a different %s" (unBreakCharacters oldName) (unBreakCharacters newName)
  | otherwise = printf "Replacing %s with %s" (unBreakCharacters oldName) (unBreakCharacters newName)
ppProspectiveChange (Removals ps) = "Removing " ++ (printThingsWithAnd . map unBreakCharacters $ ps)
