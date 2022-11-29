module Types.ProspectiveChange where

import Types.Basic
import Types.TeamOrMultiple

-- | A type to represent potential additions/replacements for my squad
data ProspectiveChange
  -- | A Player who will replace another Player in the Lineup
  = Replacement Player PlayerTeams
  -- | A Player who will fit in without displacing another Player
  | Addition PlayerTeams
  -- | No addition or replacement
  | NoChange
  -- | Removing a player
  | Removal Player
  -- | Removing multiple players in one go
  | Removals [Player]
  deriving (Eq, Show)
