module Types.BuildObject where

import Types.Lineup
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
iterativelyApplyProspectiveChanges pcs fl =
  BuildObject {buildObjectLineup = fl, buildObjectProspectiveChange = NoChange} : iterativelyApplyProspectiveChanges' pcs fl

iterativelyApplyProspectiveChanges' ::
  [ProspectiveChange] ->
  FlatLineup ->
  [BuildObject]
iterativelyApplyProspectiveChanges' [] _ = []
iterativelyApplyProspectiveChanges' (pc : pcs) fl =
  let newFL = applyProspectiveChange pc fl
   in BuildObject {buildObjectLineup = newFL, buildObjectProspectiveChange = pc} : iterativelyApplyProspectiveChanges' pcs newFL
