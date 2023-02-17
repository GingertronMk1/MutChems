-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import Classes.Data
import Data.List
import Functions.Application
import Types.Basic
import Types.Lineup
import Types.Player
import Types.Position
import Types.Printable

-- | A Prospective Change to a FlatLineup
data ProspectiveChange
  = -- | An addition to the lineup
    Addition GroupedPlayer EncodedPosition
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
        standardIndent pos
      ]
  toData (Replacement pn gp) =
    intercalate
      "\n"
      [ "# Replacement",
        standardIndent pn,
        toData gp
      ]
  toData (Removals ps) =
    intercalate
      "\n"
      [ "# Removals",
        standardIndent $ intercalate "," ps
      ]
  toData NoChange = "# NoChange"
  fromData s =
    let (kind : details) = filter (not . null) . lines $ s
     in case dropWhile (== ' ') . tail $ kind of
          "Addition" ->
            let (playersName : playersTeams : position : _) = details
             in Addition
                  (fromData . intercalate "\n" $ [playersName, playersTeams])
                  (dropSpaces position)
          "Replacement" ->
            let (replacementName : player) = details
             in Replacement
                  (dropSpaces replacementName)
                  (fromData . intercalate "\n" $ player)
          "Removals" ->
            Removals
              . splitOnInfix ","
              . dropSpaces
              . head
              $ details
          "NoChange" -> NoChange
          s' -> error . show $ (s, s')

-- | Apply a given prospective change
applyProspectiveChange :: ProspectiveChange -> FlatLineup -> FlatLineup
applyProspectiveChange NoChange fl = fl
applyProspectiveChange (Addition gp position) fl =
  let (befores, afters) = break ((== position) . show . playerPosition) fl
   in befores
        ++ ( groupedPlayerToPlayer gp (readToPositionData position) :
             afters
           )
applyProspectiveChange (Replacement oldP newP) fl =
  case break ((== oldP) . playerName) fl of
    (_, []) -> error $ printf "No player called %s" oldP
    (befores, (Player {playerPosition = oldPosition}) : afters) ->
      befores ++ (groupedPlayerToPlayer newP oldPosition : afters)
applyProspectiveChange (Removals ps) fl =
  filter ((`notElem` ps) . playerName) fl

-- | Nicely print a given prospective change
ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (GroupedPlayer {groupedPlayerName = name}) pos) =
  printf "Adding %s at %s" (unBreakCharacters name) pos
ppProspectiveChange
  ( Replacement
      oldName
      ( GroupedPlayer
          { groupedPlayerName = newName
          }
        )
    ) =
    if oldName == newName
      then
        printf
          "Replacing %s with a different %s"
          (unBreakCharacters oldName)
          (unBreakCharacters newName)
      else
        printf
          "Replacing %s with %s"
          (unBreakCharacters oldName)
          (unBreakCharacters newName)
ppProspectiveChange (Removals ps) =
  ("Removing " ++)
    . printThingsWithAnd
    . map unBreakCharacters
    $ ps
