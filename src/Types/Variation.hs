{-# LANGUAGE TupleSections #-}
-- | Module: Types.Variation
module Types.Variation where

import qualified Data.Other              as Other

import           Data.List
import           Data.Ord
import           Functions.Application
import           Types.Basic
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | One variation I can have with a Lineup.
newtype Variation
  = Variation [(Player, TeamOrMultiple)]
  deriving (Eq, Show)

instance Ord Variation where
  compare v1 v2 = case orderListOfInts (map snd $ convertFn v1) (map snd $ convertFn v2) of
    (EQ, _) -> runThroughPreferences Other.preferences v1 v2
    (c, _)  -> c
    where
      convertFn = firstAndLength
                . variationToTeams

-- | Take a list of Teams in order and give a comparison of 2 Team/Int tuples
runThroughPreferences :: [Team] -> Variation -> Variation -> Ordering
runThroughPreferences [] _ _ = EQ
runThroughPreferences (p:ps) v1 v2 = case compare (numTeam v2) (numTeam v1) of
  EQ -> runThroughPreferences ps v1 v2
  c  -> c
  where numTeam = length . filter (==p) . variationToTeams

-- | Taking a Variation and reducing it to just the list of Teams it contains
variationToTeams :: Variation -> [Team]
variationToTeams (Variation v) = sort . concatMap (expandTeamOrMultiple . snd) $ v

-- | Double fold variations - roll up all options for a given player into one PlayerTeams instance.
-- and do it for every player involved
doubleFoldVariations :: [Variation] -> [PlayerTeams]
doubleFoldVariations = sortOn fst . doubleFoldVariations' []

-- | Helper for the above.
doubleFoldVariations' ::
  -- | The accumulated list of PlayerTeams.
  [PlayerTeams] ->
  -- | The list of Variations we're working through.
  [Variation] ->
  -- | The resultant list of PlayerTeams.
  [PlayerTeams]
doubleFoldVariations' pts [] = pts
doubleFoldVariations' [] (Variation v : vs) = doubleFoldVariations' (map (\(p, t) -> (p, [t])) v) vs
doubleFoldVariations' pts (Variation v : vs) =
  let allCurrentNames = map fst pts -- Get all names
      allNewNames = filter (not . (`elem` allCurrentNames)) (map fst v) -- Get all names that we don't already have
      lengthSoFar = length . snd . head $ pts -- How many Variations have we gone through?
      newPTInit = pts ++ map (,replicate lengthSoFar NoTeam) allNewNames -- Add new names and appropriately lengthed lists of NoTeams
      newPT = map (doubleFold'' (Variation v)) newPTInit -- Add everything from the newest Variation
   in doubleFoldVariations' newPT vs -- And do the next

-- | Helper for the above.
doubleFold'' ::
  -- | The Variation we're looking at.
  Variation ->
  -- | The list of PlayerTeams we're investigating.
  PlayerTeams ->
  -- | the resultant PlayerTeams.
  PlayerTeams
doubleFold'' (Variation v) (p, ts) =
  case find ((== p) . fst) v of
    Nothing     -> (p, NoTeam : ts)
    Just (_, t) -> (p, t : ts)

-- | Convert a Lineup to its best variation
lineupToVariations :: Lineup -> Variation
lineupToVariations = Variation
                   . maximum
                   . sequence
                   . expandList

-- | Recursively iterate through the Lineup to create a Variation
-- with everyone represented
lineupToBestVariationRecursive :: Lineup -> Variation
lineupToBestVariationRecursive l = Variation
                                 . sortBy (\(a,_) (b,_) -> compareBasedOnSquad l a b)
                                 . lineupToBestVariationRecursive'
                                 $ l

-- | Helper for the above
lineupToBestVariationRecursive' :: Lineup -> [(Player, TeamOrMultiple)]
lineupToBestVariationRecursive' l =
  let convertedL = convertSquad l
      (Variation bestVariation) = lineupToVariations convertedL
   in case partition (\(_, t) -> t /= NoTeam) bestVariation of
    (nonNoTeams, []) -> nonNoTeams
    (nonNoTeams, noTeams) -> nonNoTeams ++ (lineupToBestVariationRecursive' . filter ((`elem` map fst noTeams) . fst) $ l)

-- | Generate the best Variations for a set of Lineups and add to the tuples
bestOfAllSquadsFn :: [(ProspectiveChange, Lineup)] -> [(ProspectiveChange, Lineup, Variation)]
bestOfAllSquadsFn = map bestOfOneSquadFn

-- | Generate the best Variation for a given Lineup and add it to the provided Tuple
bestOfOneSquadFn :: (ProspectiveChange, Lineup) -> (ProspectiveChange, Lineup, Variation)
bestOfOneSquadFn (c, l) = (c, l, lineupToBestVariationRecursive l)

-- | Using the totals of each team in each Variation, kind of unfolding them?.
totalsPerSquad :: [(Player, TeamOrMultiple)] -> [(Team, Int)]
totalsPerSquad =
     sortOn (Down . snd)
     . firstAndLength
     . concatMap (expandTeamOrMultiple . snd)

