-- |
-- Module: Type
module Type where

-- | Team is shorthand for a String - it is just the name of a team
type Team = String

-- | Player is shorthand for a String - it is just the name of a football player
type Player = String

-- | A player and all of their teams
type PlayerTeams = (Player, [Team])

-- | A full lineup
type Lineup = [PlayerTeams]

-- | A team and a list of all players with that team's chemistry
type TeamPlayer = (Team, [Player])

-- | An option for the whole squad's chemistries
type Option = [TeamPlayer]
