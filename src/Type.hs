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

-- | One variation I can have with a Lineup
type Variation = [(Player, TeamOrMultiple)]

-- | A team and a list of all players with that team's chemistry
type TeamPlayer = (Team, [Player])

-- | An option for the whole squad's chemistries
type Option = [TeamPlayer]

-- | Options for one or more Teams
data TeamOrMultiple = Team Team               -- ^ A single Team
                    | MultipleTeam Team Int   -- ^ A single Team with a multiplier, e.g. Raiders x3
                    | Teams [TeamOrMultiple]  -- ^ Multiple Teams, e.g. Broncos + Seahawks
                    deriving (Eq, Show)

instance Ord TeamOrMultiple where
  compare (Team t1) (Team t2) = compare t1 t2
  compare (Team t1) (MultipleTeam t2 _) = compare t1 t2
  compare t1@(Team _) (Teams t2s) = compare t1 (maximum t2s)
  compare (MultipleTeam t1 _) (Team t2) = compare t1 t2
  compare (MultipleTeam t1 _) (MultipleTeam t2 _) = compare t1 t2
  compare t1@(MultipleTeam _ _) (Teams t2s) = compare t1 (maximum t2s)
  compare (Teams t1s) t2@(Team _) = compare (maximum t1s) t2
  compare (Teams t1s) t2@(MultipleTeam _ _) = compare (maximum t1s) t2
  compare (Teams t1s) (Teams t2s) = compare (maximum t1s) (maximum t2s)