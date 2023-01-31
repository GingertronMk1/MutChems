{-# LANGUAGE DeriveGeneric #-}

-- | Module: Types.Player
module Types.Player where

import Data.Aeson
import Data.List
import Data.Ord
import Data.Teams
import GHC.Generics
import Types.Basic
import Types.TeamOrMultiple

-- * Definitions for the types that go into JSON

-- | A Grouped Player, i.e. one contained within a PositionGroup
data GroupedPlayer = GroupedPlayer
  { -- | The Player's name
    groupedPlayerName :: PlayerName,
    -- | The Player's list of TeamOrMultiples, encoded for JSON
    groupedPlayerTeams :: [EncodedTeamOrMultiple]
  }
  deriving (Eq, Show, Generic)

instance FromJSON GroupedPlayer

instance ToJSON GroupedPlayer

-- * Definitions for the types that we use for regular analysis

-- | A Player, i.e. one part of an ungrouped lineup
data Player = Player
  { playerName :: PlayerName,
    playerTeams :: [TeamOrMultiple],
    playerPosition :: Position
  }
  deriving (Eq, Ord, Show)

-- | A Variation Player, i.e. one assigned to a single TeamOrMultiple
data VariationPlayer = VariationPlayer
  { variationPlayerName :: PlayerName,
    variationPlayerTeam :: TeamOrMultiple,
    variationPlayerPosition :: Position
  }
  deriving (Eq, Ord, Show)

-- | Converting a GroupedPlayer to a regular Player
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

-- | Filter a Player's TeamOrMultiples with a list of Teams that should not be included
filterOutTeamsFromPlayer ::
  [Team] -> -- The list of banned Teams
  Player -> -- Initial Player
  Player -- Fixed Player
filterOutTeamsFromPlayer toms =
  filterPlayersTeamOrMultiples (not . teamOrMultipleContainsTeams toms)

-- | Filter a Player's TeamOrMultiples with a list of Teams that should be included
filterInTeamsFromPlayer ::
  [Team] -> -- The list of banned Teams
  Player -> -- Initial Player
  Player -- Fixed Player
filterInTeamsFromPlayer toms =
  filterPlayersTeamOrMultiples (teamOrMultipleContainsTeams toms)

-- | Filter a Player's TeamOrMultiples based on some predicate
filterPlayersTeamOrMultiples :: (TeamOrMultiple -> Bool) -> Player -> Player
filterPlayersTeamOrMultiples f p@(Player {playerTeams = ts}) =
  let ts' = case filter f ts of
        [] -> [NoTeam]
        ts'' -> ts''
  in p { playerTeams = ts'}

-- | Converting a Player to a list of VariationPlayers
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
