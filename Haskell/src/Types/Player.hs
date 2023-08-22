-- | Module: Types.Player
module Types.Player where

import Classes.Data
import Data.List
import Data.Ord
import Data.Teams
import Functions.Application
import Types.Basic
import Types.Position
import Types.Team
import Types.TeamOrMultiple

-- * Definitions for the types that go into JSON

-- | A Grouped Player, i.e. one contained within a PositionGroup
data GroupedPlayer = GroupedPlayer
  { -- | The Player's name
    groupedPlayerName :: PlayerName,
    -- | All of their available team chemistries
    groupedPlayerTeams :: [EncodedTeamOrMultiple]
  }
  deriving (Eq, Show)

instance Data GroupedPlayer where
  toData (GroupedPlayer {groupedPlayerName = name, groupedPlayerTeams = teams}) =
    intercalate "\n"
      . map standardIndent
      $ [ name,
          unwords teams
        ]
  fromData s = case filter (not . null) . lines $ s of
    (name : teams : _) ->
      GroupedPlayer
        { groupedPlayerName = dropSpaces name,
          groupedPlayerTeams = words . dropSpaces $ teams
        }
    _ -> error . show $ s

-- * Definitions for the types that we use for regular analysis

-- | A Player, i.e. one part of an ungrouped lineup
data Player = Player
  { -- | The name of the player
    playerName :: PlayerName,
    -- | All of their available team chemistries
    playerTeams :: [TeamOrMultiple],
    -- | The Player's Position
    playerPosition :: Position
  }
  deriving (Eq, Ord, Show)

-- | A Variation Player, i.e. one assigned to a single TeamOrMultiple
data VariationPlayer = VariationPlayer
  { -- | The name of the player
    variationPlayerName :: PlayerName,
    -- | One of their available TeamOrMultiples
    variationPlayerTeam :: TeamOrMultiple,
    -- | Their position
    variationPlayerPosition :: Position
  }
  deriving (Eq, Ord, Show)

-- | Converting a GroupedPlayer to a regular Player
groupedPlayerToPlayer :: GroupedPlayer -> Position -> Player
groupedPlayerToPlayer
  ( GroupedPlayer
      { groupedPlayerName = name,
        groupedPlayerTeams = teams
      }
    )
  pos =
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
  | bestT /= Legends && bestN >= 50 = (4, bestN)
  | bestT /= Legends && bestN >= 40 = (3, bestN)
  | bestT == Legends && bestN >= 40 = (2, bestN)
  | otherwise = (1, bestN)
  where
    (bestT, bestN) = maximumBy (comparing snd) cv

-- | Filter a Player's TeamOrMultiples with a list of Teams that should not be included
filterOutTeamsFromPlayer ::
  -- | The list of banned Teams
  [Team] ->
  -- | Initial Player
  Player ->
  -- | Fixed Player
  Player
filterOutTeamsFromPlayer toms =
  filterPlayersTeamOrMultiples (not . teamOrMultipleContainsTeams toms)

-- | Filter a Player's TeamOrMultiples with a list of Teams that should be included
filterInTeamsFromPlayer ::
  -- | The list of banned Teams
  [Team] ->
  -- | Initial Player
  Player ->
  -- Fixed Player
  Player
filterInTeamsFromPlayer toms =
  filterPlayersTeamOrMultiples (teamOrMultipleContainsTeams toms)

-- | Filter a Player's TeamOrMultiples based on some predicate
filterPlayersTeamOrMultiples :: (TeamOrMultiple -> Bool) -> Player -> Player
filterPlayersTeamOrMultiples f p@(Player {playerTeams = ts}) =
  let ts' = case filter f ts of
        [] -> [NoTeam]
        ts'' -> ts''
   in p {playerTeams = ts'}

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

-- | Does a Player belong to a given Team
doesPlayerBelongToTeam :: Team -> Player -> Bool
doesPlayerBelongToTeam t =
  not
    . null
    . playerTeams
    . filterInTeamsFromPlayer [t]
