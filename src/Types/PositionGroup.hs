{-# LANGUAGE DeriveGeneric #-}

module Types.PositionGroup where

import Data.Aeson
import GHC.Generics
import Types.Basic
import Types.Player

data PositionGroup = PositionGroup
  { positionGroupPosition :: Position,
    positionGroupPlayers :: [GroupedPlayer]
  }
  deriving (Eq, Show, Generic)

instance FromJSON PositionGroup

instance ToJSON PositionGroup
