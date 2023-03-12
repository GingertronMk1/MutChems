-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import Classes.Data
import Data.Char
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
    Replacement [PlayerName] GroupedPlayer
  | -- | No change
    NoChange
  deriving (Show)

instance Data ProspectiveChange where
  toData (Addition gp pos) =
    intercalate
      "\n"
      [ "# Addition",
        standardIndent pos,
        toData gp
      ]
  toData (Replacement pn gp) =
    intercalate
      "\n"
      [ "# Replacement",
        standardIndent (intercalate "," pn),
        toData gp
      ]
  toData NoChange = "# NoChange"
  fromData s =
    let (kind : details) = filter (not . null) . lines $ s
     in case dropWhile isSpace . tail $ kind of
          "Addition" ->
            let (position : playersName : playersTeams : _) = details
             in Addition
                  (fromData . intercalate "\n" $ [playersName, playersTeams])
                  (dropSpaces position)
          "Replacement" ->
            let (replacementName : player) = details
             in Replacement
                  (splitOnInfix "," . dropSpaces $ replacementName)
                  (fromData . intercalate "\n" $ player)
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
applyProspectiveChange (Replacement [] _) fl = fl
applyProspectiveChange (Replacement oldPs@(oldP : _) newP) fl =
  case break ((== oldP) . playerName) fl of
    (_, []) -> error $ printf "No player called %s" (show oldP)
    (befores, (Player {playerPosition = oldPosition}) : afters) ->
      let filterOldPs = filter ((`notElem` oldPs) . playerName)
       in filterOldPs befores
            ++ (groupedPlayerToPlayer newP oldPosition : filterOldPs afters)

-- | Nicely print a given prospective change
ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (GroupedPlayer {groupedPlayerName = name}) pos) =
  printf "Adding %s at %s" (unBreakCharacters name) pos
ppProspectiveChange (Replacement [] _) = "Not actually changing anything"
ppProspectiveChange
  ( Replacement
      oldNames@(oldName : _)
      ( GroupedPlayer
          { groupedPlayerName = newName
          }
        )
    ) =
    let formatString =
          if oldName == newName
            then "Replacing %s with a different %s"
            else "Replacing %s with %s"
     in printf
          formatString
          (printThingsWithAnd . map unBreakCharacters $ oldNames)
          (unBreakCharacters newName)
