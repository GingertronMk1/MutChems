-- | Module: Types.ProspectiveChange
module Types.ProspectiveChange where

import           Functions.Application
import           Types.Basic
import           Types.TeamOrMultiple

-- | A type to represent potential additions/replacements for my squad
data ProspectiveChange
  -- | A Player who will replace another Player in the Lineup
  = Replacement PlayerName Player
  -- | A Player who will fit in without displacing another Player
  | Addition Player
  -- | No addition or replacement
  | NoChange
  -- | Removing a player
  | Removal PlayerName
  deriving (Eq, Show)

-- * Functions to add prospective changes to a Lineup

-- | Add each ProspectiveChange in turn to the squad, keeping the initial squad
addProspectivesInTurn :: [ProspectiveChange] -> Lineup -> [(ProspectiveChange, Lineup)]
addProspectivesInTurn ps l = (NoChange, l) : addProspectivesInTurn' ps l

-- | Add the remaining prospectives in turn
addProspectivesInTurn' :: [ProspectiveChange] -> Lineup -> [(ProspectiveChange, Lineup)]
addProspectivesInTurn' [] _ = []
addProspectivesInTurn' (p:ps) l =
  let newL = checkLineupIsValid . addProspective p . checkLineupIsValid $ l
   in (p, newL) : addProspectivesInTurn' ps newL

-- | Add a single prospective addition to the squad.
-- Throw an error if we're trying to replace someone who doesn't exist.
addProspective :: ProspectiveChange -> Lineup -> Lineup
addProspective NoChange l = l
addProspective (Addition p@(P {pPosition = additionPosition})) l =
  let (befores, firstPosition:afters) = break (\P {pPosition = playerPosition} -> playerPosition == additionPosition) l
   in befores ++ (p:firstPosition:afters)
addProspective (Replacement p newP) l =
  case break (\P {pName = playerName} -> p == playerName) l of
    (_, [])                                -> error $ printf "No player called %s in lineup" [p]
    (firstPart, P {pPosition = oldPosition}:theRest) -> firstPart ++ (newP {pPosition = oldPosition}:theRest)
addProspective (Removal p) l = filter (\P {pName = playerName} -> playerName == p) l

-- | Nicely print a Prospective Change
ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (P {pName = p})) = printf "Adding %s" [p]
ppProspectiveChange (Replacement p1 (P {pName = p2})) = printf "Replacing %s with %s" [p1, p2]
ppProspectiveChange (Removal p) = printf "Getting rid of %s" [p]

