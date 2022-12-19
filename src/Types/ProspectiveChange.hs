-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import           Functions.Application
import           Types.Basic
import           Types.TeamOrMultiple

-- | A type to represent potential additions/replacements for my squad
data ProspectiveChange
  -- | A Player who will replace another Player in the Lineup
  = Replacement Player PlayerObject
  -- | A Player who will fit in without displacing another Player
  | Addition PlayerObject
  -- | No addition or replacement
  | NoChange
  -- | Removing a player
  | Removal Player
  deriving (Eq, Show)

-- * Functions to add prospective changes to a Lineup

-- | Add each ProspectiveChange in turn to the squad, keeping the initial squad
addProspectivesInTurn :: [ProspectiveChange] -> LineupObject -> [(ProspectiveChange, LineupObject)]
addProspectivesInTurn ps l = (NoChange, l) : addProspectivesInTurn' ps l

-- | Add the remaining prospectives in turn
addProspectivesInTurn' :: [ProspectiveChange] -> LineupObject -> [(ProspectiveChange, LineupObject)]
addProspectivesInTurn' [] _ = []
addProspectivesInTurn' (p:ps) l =
  let newL = addProspective p l
   in (p, newL) : addProspectivesInTurn' ps newL

-- | Add a single prospective addition to the squad.
-- Throw an error if we're trying to replace someone who doesn't exist.
addProspective :: ProspectiveChange -> LineupObject -> LineupObject
addProspective NoChange l = l
addProspective (Addition p@(P {position = additionPosition})) l =
  let (befores, firstPosition:afters) = break (\P {position = playerPosition} -> playerPosition == additionPosition) l
   in befores ++ (p:firstPosition:afters)
addProspective (Replacement p newP) l =
  case break (\P {name = playerName} -> p == playerName) l of
    (_, [])                                -> error $ printf "No player called %s in lineup" [p]
    (firstPart, P {position = oldPosition}:theRest) -> firstPart ++ (newP {position = oldPosition}:theRest)
addProspective (Removal p) l = filter (\P {name = playerName} -> playerName == p) l

-- | Nicely print a Prospective Change
ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (P {name = p})) = printf "Adding %s" [p]
ppProspectiveChange (Replacement p1 (P {name = p2})) = printf "Replacing %s with %s" [p1, p2]
ppProspectiveChange (Removal p) = printf "Getting rid of %s" [p]

