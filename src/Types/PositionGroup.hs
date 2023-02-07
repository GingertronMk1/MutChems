-- | Module: Types.PositionGroup
module Types.PositionGroup where

import Classes.Data
import Data.List
import Functions.Application
import Types.Basic
import Types.Player

-- | The Position Group, with a Position and a list of Players
data PositionGroup = PositionGroup
  { -- | The Players' Position
    positionGroupPosition :: Position,
    -- | The Players in that Position
    positionGroupPlayers :: [GroupedPlayer]
  }
  deriving (Eq, Show, Read)

instance Data PositionGroup where
  toData (PositionGroup {positionGroupPosition = pos, positionGroupPlayers = pla}) =
    let playerDatas = intercalate "\n    ---\n" . map toData $ pla
        positionData = "# " ++ pos
     in positionData ++ "\n" ++ playerDatas
  fromData s =
    let (pos, playerList) = break (== '\n') s
     in case splitAt 2 pos of
          ("# ", pos') ->
            PositionGroup
              { positionGroupPosition = pos',
                positionGroupPlayers = map fromData . splitOnInfix "    ---" $ playerList
              }
          _ -> error pos
