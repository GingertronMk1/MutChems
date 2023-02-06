{-# LANGUAGE DeriveGeneric #-}

-- | Module: Types.PositionGroup
module Types.PositionGroup where

import Data.Aeson
import GHC.Generics
import Types.Basic
import Types.Player

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
