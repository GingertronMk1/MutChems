-- | Module: Types.Variation
module Types.Variation where

import           Data.List
import           Data.Ord
import           Data.Teams
import           Functions.Application
import           Types.Basic
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | One variation I can have with a Lineup.
newtype Variation
  = Variation [(Player, TeamOrMultiple, Position)]
  deriving (Eq, Show)

instance Ord Variation where
  compare v1 v2 = case orderListOfInts (convertFn v1) (convertFn v2) of
    (EQ, _) -> runThroughPreferences preferences v1 v2
    (c, _)  -> c
    where
      convertFn = map snd
                . firstAndLength
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
variationToTeams (Variation v) = sort . concatMap (expandTeamOrMultiple . getSecond) $ v

-- | Convert a Lineup to its best variation
lineupToVariations :: Lineup -> Variation
lineupToVariations = Variation
                   . maximum
                   . sequence
                   . expandLineup

-- | Recursively iterate through the Lineup to create a Variation
-- with everyone represented
lineupToBestVariationRecursive :: Lineup -> Variation
lineupToBestVariationRecursive l = Variation
                                 . sortBy (\(a,_,_) (b,_,_) -> compareBasedOnSquad l a b)
                                 . lineupToBestVariationRecursive'
                                 $ l

-- | Helper for the above
lineupToBestVariationRecursive' :: Lineup -> [(Player, TeamOrMultiple, Position)]
lineupToBestVariationRecursive' l =
  let convertedL = convertSquad l
      (Variation bestVariation) = lineupToVariations convertedL
   in case partition (\(_, t,_) -> t /= NoTeam) bestVariation of
    (nonNoTeams, []) -> nonNoTeams
    (nonNoTeams, noTeams) -> nonNoTeams ++ (lineupToBestVariationRecursive' . filter ((`elem` map getFirst noTeams) . getFirst) $ l)

-- | Generate the best Variations for a set of Lineups and add to the tuples
bestOfAllSquadsFn :: [(ProspectiveChange, Lineup)] -> [(ProspectiveChange, Lineup, Variation)]
bestOfAllSquadsFn = map bestOfOneSquadFn

-- | Generate the best Variation for a given Lineup and add it to the provided Tuple
bestOfOneSquadFn :: (ProspectiveChange, Lineup) -> (ProspectiveChange, Lineup, Variation)
bestOfOneSquadFn (c, l) = (c, l, lineupToBestVariationRecursive l)

-- | Using the totals of each team in each Variation, kind of unfolding them?.
totalsPerSquad :: [(Player, TeamOrMultiple, Position)] -> [(Team, Int)]
totalsPerSquad = sortOn (Down . snd)
              . firstAndLength
              . concatMap (expandTeamOrMultiple . getSecond)

