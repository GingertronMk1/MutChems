{-# LANGUAGE TupleSections #-}
module Types.Variation where

import           Data.List
import           Data.Teams
import           Functions.Application
import           Types.Basic
import           Types.TeamOrMultiple

-- | One variation I can have with a Lineup.
newtype Variation
  = Variation [(Player, TeamOrMultiple)]
  deriving (Eq, Show)

instance Ord Variation where
  compare (Variation v1) (Variation v2) = case orderListOfInts (map snd $ convertFn v1) (map snd $ convertFn v2) of
    (EQ, _) -> runThroughPreferences [legends, seahawks, eagles] convertedV1 convertedV2
    (c, _) -> c
    where
      convertedV1 = convertFn v1
      convertedV2 = convertFn v2
      convertFn = map firstAndLength
                . group
                . sort
                . concatMap (expandTeamOrMultiple . snd)


runThroughPreferences :: [Team] -> [(Team, Int)] -> [(Team, Int)] -> Ordering
runThroughPreferences [] _ _ = EQ
runThroughPreferences (p:ps) v1 v2 = case compare (numTeam v2) (numTeam v1) of
  EQ -> runThroughPreferences ps v1 v2
  c  -> c
  where numTeam tns = case find (\(t,_) -> t == p) tns of
          Just (_,n) -> n
          Nothing    -> 0

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

-- | Convert a Lineup to all of its top 10 variations
lineupToVariations :: Lineup -> [Variation]
lineupToVariations = take 10
                   . maximumValues
                   . map (Variation . sortOn snd)
                   . sequence
                   . expandList
