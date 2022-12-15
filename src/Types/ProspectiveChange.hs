-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import           Functions.Application
import           Types.Basic
import           Types.TeamOrMultiple

-- | A type to represent potential additions/replacements for my squad
data ProspectiveChange
  -- | A Player who will replace another Player in the Lineup
  = Replacement Player PlayerTeams
  -- | A Player who will fit in without displacing another Player
  | Addition PlayerTeamsPosition
  -- | No addition or replacement
  | NoChange
  -- | Removing a player
  | Removal Player
  deriving (Eq, Show)

-- * Functions to add prospective changes to a Lineup

-- | Add each ProspectiveChange in turn to the squad, keeping the initial squad
addProspectivesInTurn :: [ProspectiveChange] -> Lineup -> [(ProspectiveChange, Lineup)]
addProspectivesInTurn ps l = (NoChange, l) : addProspectivesInTurn' ps l

-- | Add the remaining prospectives in turn
addProspectivesInTurn' :: [ProspectiveChange] -> Lineup -> [(ProspectiveChange, Lineup)]
addProspectivesInTurn' [] _ = []
addProspectivesInTurn' (p:ps) l =
  let newL = addProspective p l
   in (p, newL) : addProspectivesInTurn' ps newL

-- | Add a single prospective addition to the squad.
-- Throw an error if we're trying to replace someone who doesn't exist.
addProspective :: ProspectiveChange -> Lineup -> Lineup
addProspective NoChange l = l
addProspective (Addition pt) l =
  let (befores, firstPosition:afters) = break ((==getThird pt) . getThird) l
   in befores ++ (pt:firstPosition:afters)
addProspective (Replacement p (newP, newPTs)) l =
  case break ((==p) . getFirst) l of
    (_, [])                                -> error $ printf "No player called %s in lineup" [p]
    (firstPart, (_,_,oldPosition):theRest) -> firstPart ++ ((newP, newPTs, oldPosition):theRest)
addProspective (Removal p) l = filter ((/=p) . getFirst) l
