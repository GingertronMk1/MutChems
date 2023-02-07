{-# LANGUAGE DeriveGeneric #-}

-- | Module: Types.PositionGroup
module Types.PositionGroup where

import Data.Aeson
import GHC.Generics
import Types.Basic
import Types.Player
import Classes.Data
import Data.List (intercalate)

-- | The Position Group, with a Position and a list of Players
data PositionGroup = PositionGroup
  { -- | The Players' Position
    positionGroupPosition :: Position,
    -- | The Players in that Position
    positionGroupPlayers :: [GroupedPlayer]
  }
  deriving (Eq, Show, Generic, Read)

instance FromJSON PositionGroup

instance ToJSON PositionGroup

instance Data PositionGroup where
  toData (PositionGroup { positionGroupPosition = pos, positionGroupPlayers = pla })=
    let playerDatas = intercalate "    ---\n" . map toData $ pla
        positionData = "# " ++ pos
     in positionData ++ "\n" ++ playerDatas
  fromData s = let (pos:players) = lines s
    in PositionGroup {positionGroupPosition=drop 2 pos, positionGroupPlayers=map fromData players}