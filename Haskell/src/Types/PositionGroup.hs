-- | Module: Types.PositionGroup
module Types.PositionGroup where

import Classes.Data
import Data.List
import Functions.Application
import Types.Basic
import Types.Player
import Types.Printable

-- | The Position Group, with a Position and a list of Players
data PositionGroup = PositionGroup
  { -- | The Players' Position
    positionGroupPosition :: EncodedPosition,
    -- | The Players in that Position
    positionGroupPlayers :: [GroupedPlayer]
  }
  deriving (Eq, Show)

instance Data PositionGroup where
  toData (PositionGroup {positionGroupPosition = pos, positionGroupPlayers = pla}) =
    printf
      "# %s\n%s"
      pos
      ( intercalate ("\n" ++ standardIndent "---" ++ "\n")
          . map toData
          $ pla
      )
  fromData s =
    let (pos, playerList) = break (== '\n') s
     in case splitAt 2 pos of
          ("# ", pos') ->
            PositionGroup
              { positionGroupPosition = pos',
                positionGroupPlayers =
                  map fromData
                    . splitOnInfix (standardIndent "---")
                    $ playerList
              }
          _ -> error pos