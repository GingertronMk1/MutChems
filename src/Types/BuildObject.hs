-- | Module: Types.BuildObject
module Types.BuildObject where

import Types.Lineup
import Types.Player
import Types.ProspectiveChange
import Types.Team

-- | The Build Object, containing a Lineup and a Prospective Change
data BuildObject = BuildObject
  { -- | The lineup
    buildObjectLineup :: FlatLineup,
    -- | The prospective change
    buildObjectProspectiveChange :: ProspectiveChange
  }
  deriving (Show)

-- | Apply the list of Prospective Changes, starting with a NoChange
iterativelyApplyProspectiveChanges ::
  [ProspectiveChange] ->
  FlatLineup ->
  [BuildObject]
iterativelyApplyProspectiveChanges pcs fl =
  scanl
    stepBuildObject
    ( BuildObject
        { buildObjectLineup = fl,
          buildObjectProspectiveChange = NoChange
        }
    )
    pcs

-- | "Stepping" a BuildObject - applying a ProspectiveChange to the FlatLineup
-- contained within
stepBuildObject :: BuildObject -> ProspectiveChange -> BuildObject
stepBuildObject (BuildObject {buildObjectLineup = currLineup}) pc =
  BuildObject
    { buildObjectLineup = applyProspectiveChange pc currLineup,
      buildObjectProspectiveChange = pc
    }

-- | Filter out some Teams from a BuildObject
filterOutTeams :: [Team] -> BuildObject -> BuildObject
filterOutTeams ts bo@(BuildObject {buildObjectLineup = l}) =
  bo {buildObjectLineup = map (filterOutTeamsFromPlayer ts) l}
