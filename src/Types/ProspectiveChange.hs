-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import Functions.Application
import Text.Printf
import Types.Basic
import Types.TeamOrMultiple

-- | A type to represent potential additions/replacements for my squad
data ProspectiveChange
  = -- | A Player who will replace another Player in the Lineup
    Replacement PlayerName Player
  | -- | A Player who will fit in without displacing another Player
    Addition Player
  | -- | No addition or replacement
    NoChange
  | -- | Removing a player
    Removals [PlayerName]
  deriving (Eq, Show)

-- | An object containing a Lineup and a ProspectiveChange that has led to that
-- Lineup
data BuildObject = BuildObject
  { buildObjectLineup :: Lineup,
    buildObjectProspectiveChange :: ProspectiveChange
  }
  deriving (Eq, Show)

-- * Functions to add prospective changes to a Lineup

-- | Add each ProspectiveChange in turn to the squad, keeping the initial squad
addProspectivesInTurn :: [ProspectiveChange] -> Lineup -> [BuildObject]
addProspectivesInTurn ps l = BuildObject {buildObjectLineup = l, buildObjectProspectiveChange = NoChange} : addProspectivesInTurn' ps l

-- | Add the remaining prospectives in turn
addProspectivesInTurn' :: [ProspectiveChange] -> Lineup -> [BuildObject]
addProspectivesInTurn' [] _ = []
addProspectivesInTurn' (p : ps) l =
  let newL = checkLineupIsValid . addProspective p . checkLineupIsValid $ l
   in BuildObject {buildObjectLineup = newL, buildObjectProspectiveChange = p} : addProspectivesInTurn' ps newL

-- | Add a single prospective addition to the squad.
-- Throw an error if we're trying to replace someone who doesn't exist.
addProspective :: ProspectiveChange -> Lineup -> Lineup
addProspective NoChange l = l
addProspective (Addition p@(P {pPosition = additionPosition})) l =
  case break ((additionPosition ==) . pPosition) l of
    (befores, []) -> befores ++ [p]
    (befores, firstPosition : afters) -> befores ++ (p : firstPosition : afters)
addProspective (Replacement p newP) l =
  case break ((p ==) . pName) l of
    (_, []) -> error $ printf "No player called %s in lineup" p
    (firstPart, P {pPosition = oldPosition} : theRest) -> firstPart ++ (newP {pPosition = oldPosition} : theRest)
addProspective (Removals p) l = filter ((`notElem` p) . pName) l

-- | Nicely print a Prospective Change
ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (P {pName = p})) = printf "Adding %s" p
ppProspectiveChange (Replacement p1 (P {pName = p2}))
  | p1 == p2 = printf "Replacing %s with a different %s" p1 p2
  | otherwise = printf "Replacing %s with %s" p1 p2
ppProspectiveChange (Removals p) = printf "Getting rid of %s" (printThingsWithAnd p)
