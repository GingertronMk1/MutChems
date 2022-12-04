-- | Module: Types.Basic
--
-- Alias types
module Types.Basic where

-- | Team is shorthand for a String - it is just the name of a team.
type Team = String

-- | Player is shorthand for a String - it is just the name of a football player.
type Player = String

-- | A team and a list of all players with that team's chemistry.
type TeamPlayer = (Team, [Player])

-- | An option for the whole squad's chemistries.
type Option = [TeamPlayer]

