{-# LANGUAGE DeriveGeneric #-}

module Types.Player where

import Data.Aeson
import Data.List
import Data.List.Split
import Data.Ord
import Data.Teams
import Functions.Application
import GHC.Generics
import Text.Printf
import Types.Basic
import Types.TeamOrMultiple

-- * Definitions for the types that go into JSON

data GroupedPlayer = GroupedPlayer
  { groupedPlayerName :: PlayerName,
    groupedPlayerTeams :: [EncodedTeamOrMultiple]
  }
  deriving (Eq, Show, Generic)

instance FromJSON GroupedPlayer

instance ToJSON GroupedPlayer

-- * Definitions for the types that we use for regular analysis

data Player = Player
  { playerName :: PlayerName,
    playerTeams :: [TeamOrMultiple],
    playerPosition :: Position
  }
  deriving (Eq, Ord, Show)

data VariationPlayer = VariationPlayer
  { variationPlayerName :: PlayerName,
    variationPlayerTeam :: TeamOrMultiple,
    variationPlayerPosition :: Position
  }
  deriving (Eq, Ord, Show)

groupedPlayerToPlayer :: GroupedPlayer -> Position -> Player
groupedPlayerToPlayer (GroupedPlayer {groupedPlayerName = name, groupedPlayerTeams = teams}) pos =
  Player
    { playerName = name,
      playerTeams = decodeTeamOrMultiples teams,
      playerPosition = pos
    }

-- * The Variation

-- | Take a Team and how many there are and convert it into an integer so we can
-- more easily compare it to others - ordering them in priority
toNumerical :: [(Team, Int)] -> (Int, Int)
toNumerical cv
  | bestT /= legends && bestN >= 50 = (4, bestN)
  | bestT /= legends && bestN >= 40 = (3, bestN)
  | bestT == legends && bestN >= 40 = (2, bestN)
  | otherwise = (1, bestN)
  where
    (bestT, bestN) = maximumBy (comparing snd) cv


reducePlayerTeams ::
  [Team] -> -- The list of banned TeamOrMultiples
  Player -> -- Initial Player
  Player -- Fixed Player
reducePlayerTeams toms p@(Player {playerTeams = ts}) =
  let ts' = case filter (teamOrMultipleContainsTeams toms) ts of
        [] -> [NoTeam]
        ts'' -> ts''
   in p {playerTeams = ts'}

playerToVariationPlayers :: Player -> [VariationPlayer]
playerToVariationPlayers
  ( Player
      { playerName = pName,
        playerTeams = pTeams,
        playerPosition = pPosition
      }
    ) =
    [ VariationPlayer
        { variationPlayerName = pName,
          variationPlayerTeam = pTeam,
          variationPlayerPosition = pPosition
        }
      | pTeam <- pTeams
    ]
