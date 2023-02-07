-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import Classes.Data
import Data.List
import Functions.Application
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
  deriving (Show)

instance Data ProspectiveChange where
  toData (Addition gp pos) =
    intercalate
      "\n"
      [ "# Addition",
        toData gp,
        "    " ++ pos
      ]
  toData (Replacement pn gp) =
    intercalate
      "\n"
      [ "# Replacement",
        "    " ++ pn,
        toData gp
      ]
  toData (Removals ps) =
    intercalate
      "\n"
      [ "# Removals",
        "    " ++ intercalate "," ps
      ]
  toData NoChange = "# NoChange"
  fromData s = case filter (not . null) . lines $ s of
    ("# Addition" : playersName : playersTeams : position : _) ->
      Addition
        (fromData . intercalate "\n" $ [playersName, playersTeams])
        (dropWhile (== ' ') position)
    ("# Replacement" : replacementName : player) ->
      Replacement
        (dropWhile (== ' ') replacementName)
        (fromData . intercalate "\n" $ player)
    ("# Removals" : ls) ->
      let players = head ls
       in Removals $ splitOnInfix "," . dropWhile (== ' ') $ players
    ("# NoChange" : _) -> NoChange
    s' -> error . show $ (s, s')

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
