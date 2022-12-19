-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import           Data.Positions
import           Data.Squad
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | The squad with the team strategy item sorted
processedStrategy :: [PlayerObject]
processedStrategy = map (\s -> emptyPlayer {name = "STRATEGY: " ++ ppTeamOrMultiple s, teams = [s], position = strategyCard}) strategy

-- | Just the base squad and strategy item
squadNoProspectives :: LineupObject
squadNoProspectives = filter (\P {teams = ts} -> not (null ts))
                    . (++processedStrategy)
                    . streamlineLineup
                    $ baseSquad

-- | The generated list of squads in "chronological" order (or at least planned)
iteratedProspectiveSquads :: [(ProspectiveChange, LineupObject)]
iteratedProspectiveSquads =  addProspectivesInTurn prospectiveAdditions
                          $ squadNoProspectives
