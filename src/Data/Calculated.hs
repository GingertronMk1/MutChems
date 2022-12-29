-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import Data.Positions
import Data.Squad
import Types.ProspectiveChange
import Types.TeamOrMultiple

-- | The squad with the team strategy item sorted
processedStrategy :: [Player]
processedStrategy = case strategy of
  NoTeam -> []
  tom ->
    [ emptyPlayer
        { pName = "STRATEGY: " ++ ppTeamOrMultiple tom,
          pTeams = [tom],
          pPosition = strategyCard
        }
    ]

-- | Just the base squad and strategy item
squadNoProspectives :: Lineup
squadNoProspectives =
  filter (not . null . pTeams)
    . (++ processedStrategy)
    . streamlineLineup
    $ baseSquad

-- | The generated list of squads in "chronological" order (or at least planned)
iteratedProspectiveSquads :: [BuildObject]
iteratedProspectiveSquads =
  addProspectivesInTurn
    prospectiveAdditions
    squadNoProspectives
