-- | Module: Types.BuildObject
module Types.BuildObject where

import Types.Basic
import Types.Lineup
import Types.Player
import Types.ProspectiveChange

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
iterativelyApplyProspectiveChanges pcs =
  iterativelyApplyProspectiveChanges' (NoChange : pcs)

-- | Apply the list of Prospective Changes to an initial FlatLineup and return the
-- resultant list of BuildObjects
iterativelyApplyProspectiveChanges' ::
  [ProspectiveChange] ->
  FlatLineup ->
  [BuildObject]
iterativelyApplyProspectiveChanges' [] _ = []
iterativelyApplyProspectiveChanges' (pc : pcs) fl =
  let bo = genBuildObject pc fl
   in bo : (iterativelyApplyProspectiveChanges' pcs . buildObjectLineup $ bo)

-- | Take a ProspectiveChange and a FlatLineup and create a BuildObject from the
-- result
genBuildObject :: ProspectiveChange -> FlatLineup -> BuildObject
genBuildObject pc fl =
  BuildObject
    { buildObjectLineup = applyProspectiveChange pc fl,
      buildObjectProspectiveChange = pc
    }

-- | Filter out some Teams from a BuildObject
filterOutTeams :: [Team] -> BuildObject -> BuildObject
filterOutTeams ts bo@(BuildObject {buildObjectLineup = l}) =
  bo {buildObjectLineup = map (filterOutTeamsFromPlayer ts) l}
