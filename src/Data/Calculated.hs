-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import           Data.Bifunctor
import           Data.Positions
import           Data.Squad
import           Functions.Display
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | The squad with the team strategy item sorted
processedStrategy :: Lineup
processedStrategy = case strategy of
  NoTeam -> []
  s      -> [("STRATEGY: " ++ ppTeamOrMultiple s, [s], strategyCard)]

-- | Just the base squad and strategy item
squadNoProspectives :: Lineup
squadNoProspectives = filter (\(_,ts,_) -> not (null ts))
                    . (++processedStrategy)
                    . concatMap expandPosition
                    $ baseSquad

-- | The generated list of squads in "chronological" order (or at least planned)
iteratedProspectiveSquads :: [(ProspectiveChange, Lineup)]
iteratedProspectiveSquads = map (second convertSquad)
                          . addProspectivesInTurn prospectiveAdditions
                          $ squadNoProspectives