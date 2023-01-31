module Types.BuildObject where

import Types.Basic
import Types.Lineup
import Types.Player
import Types.ProspectiveChange

data BuildObject = BuildObject
  { buildObjectLineup :: FlatLineup,
    buildObjectProspectiveChange :: ProspectiveChange
  }
  deriving (Show)

iterativelyApplyProspectiveChanges ::
  [ProspectiveChange] ->
  FlatLineup ->
  [BuildObject]
iterativelyApplyProspectiveChanges pcs =
  iterativelyApplyProspectiveChanges' (NoChange : pcs)

iterativelyApplyProspectiveChanges' ::
  [ProspectiveChange] ->
  FlatLineup ->
  [BuildObject]
iterativelyApplyProspectiveChanges' [] _ = []
iterativelyApplyProspectiveChanges' (pc : pcs) fl =
  let bo = genBuildObject pc fl
   in bo : (iterativelyApplyProspectiveChanges' pcs . buildObjectLineup $ bo)

genBuildObject :: ProspectiveChange -> FlatLineup -> BuildObject
genBuildObject pc fl =
  BuildObject
    { buildObjectLineup = applyProspectiveChange pc fl,
      buildObjectProspectiveChange = pc
    }

filterOutTeams :: [Team] -> BuildObject -> BuildObject
filterOutTeams ts bo@(BuildObject {buildObjectLineup = l}) =
  bo {buildObjectLineup = map (filterOutTeamsFromPlayer ts) l}
