{-# LANGUAGE DeriveGeneric #-}
module Types.PositionGroup where

import Data.Aeson
import Types.Basic
import Types.Player
import GHC.Generics

data PositionGroup = PositionGroup
  { positionGroupPosition :: Position,
    positionGroupPlayers :: [GroupedPlayer]
  }
  deriving (Eq, Show, Generic)

instance FromJSON PositionGroup

instance ToJSON PositionGroup

