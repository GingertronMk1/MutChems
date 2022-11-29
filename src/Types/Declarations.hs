-- |
-- Module: Type
module Types.Declarations where


-- | Team is shorthand for a String - it is just the name of a team.
type Team = String

-- | Player is shorthand for a String - it is just the name of a football player.
type Player = String

-- | A player and all of their teams.
type PlayerTeams = (Player, [TeamOrMultiple])

-- | A full lineup.
type Lineup = [PlayerTeams]

-- | One variation I can have with a Lineup.
newtype Variation
  = Variation [(Player, TeamOrMultiple)]
  deriving (Eq, Show)

-- | A team and a list of all players with that team's chemistry.
type TeamPlayer = (Team, [Player])

-- | An option for the whole squad's chemistries.
type Option = [TeamPlayer]

-- | Options for one or more Teams.
data TeamOrMultiple
  -- | Null value.
  = NoTeam
  -- | A single Team.
  | Team Team
  -- | A single Team with a multiplier, e.g. Raiders x3.
  | MultipleTeam Team Int
  -- | Multiple Teams, e.g. Broncos + Seahawks.
  | Teams [TeamOrMultiple]
  deriving (Eq, Show)


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
