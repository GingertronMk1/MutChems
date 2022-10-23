-- |
-- Module: Type
module Type where

-- | Team is shorthand for a String - it is just the name of a team
type Team = String

-- | Player is shorthand for a String - it is just the name of a football player
type Player = String

-- | A player and all of their teams
type PlayerTeams = (Player, [TeamOrMultiple])

-- | A full lineup
type Lineup = [PlayerTeams]

-- | A team and a list of all players with that team's chemistry
type TeamPlayer = (Team, [Player])

-- | An option for the whole squad's chemistries
type Option = [TeamPlayer]

-- | Options for one or more Teams
data TeamOrMultiple = Team Team                           -- ^ A single Team
                    | MultipleTeam Team Int               -- ^ A single Team with a multiplier, e.g. Raiders x3
                    | Teams TeamOrMultiple TeamOrMultiple -- ^ Multiple Teams, e.g. Broncos + Seahawks
                    deriving (Eq, Show)