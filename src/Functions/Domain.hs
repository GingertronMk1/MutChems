{-# LANGUAGE TupleSections #-}
-- |
-- Module: Functions.Domain
--
-- Domain functions, i.e. those which are more specific to this project and the
-- data structures it contains
module Functions.Domain where

-- My imports

-- Haskell imports
import           Data.Bifunctor
import           Data.List
import           Data.Maybe
import qualified Data.Teams            as T
import           Functions.Application
import           Type

-- | does a given TeamOrMultiple contain a given Team
includesTeam ::
  -- | The Team being searched for
  Team ->
  -- | The TeamOrMultiple being searched
  TeamOrMultiple ->
  -- | Does it contain?
  Bool
includesTeam t = elem t . expandTeamOrMultiple

-- | How many options do we get from a given Lineup?
numberOfOptionsFn :: Lineup -> Int
numberOfOptionsFn = product . map (length . snd)

-- | Give me a list of all Teams in a given Lineup
allTeamsFn :: Lineup -> [Team]
allTeamsFn =
  concatMap expandTeamOrMultiple
    . concatMap snd

-- | Filtering a Lineup to contain only those Teams with 3 or more entries
filteredSquadFn :: Lineup -> Lineup
filteredSquadFn s =
  let allTeams = allTeamsFn s
      numberOfOneTeam t = length . filter (== t) $ allTeams
      filterFn' t = numberOfOneTeam t > 3 || t == T.all32Teams
      filterFn NoTeam             = False
      filterFn (Team t)           = filterFn' t
      filterFn (MultipleTeam t _) = filterFn' t
      filterFn (Teams ts)         = any filterFn ts
   in filter (not . null . snd) . map (Data.Bifunctor.second (filter filterFn)) $ s

-- | Change all players with all 32 teams to contain all useful teams
-- useful here being "all other actual teams" - there's no point giving him
-- the option for Jacksonville if nobody else has ever played for them
convertAll32Teams :: Lineup -> Lineup
convertAll32Teams l =
  let allTeams =
        rmDups
          . filter (/= T.all32Teams)
          . allTeamsFn
          $ l
   in map (second $ concatMap $ convertSingle allTeams) l

-- | Convert a single TeamOrMultiple to a list of Teams should that TeamOrMultiple
-- be all 32 teams
convertSingle ::
  -- | The list of Teams that should be considered for conversion
  [Team] ->
  -- | The TeamOrMultiple being converted
  TeamOrMultiple ->
  -- | The resultant list
  [TeamOrMultiple]
convertSingle _ NoTeam = []
convertSingle ts team@(Team t) =
  if t == T.all32Teams
    then map Team ts
    else [team]
convertSingle ts (MultipleTeam t i) =
  if t == T.all32Teams
    then map (`MultipleTeam` i) ts
    else [MultipleTeam t i]
convertSingle ts (Teams t) = concatMap (convertSingle ts) t

-- | Double fold variations - roll up all options for a given player into one PlayerTeams instance
-- and do it for every player involved
doubleFoldVariations :: [Variation] -> [PlayerTeams]
doubleFoldVariations = sortOn fst . doubleFoldVariations' []

-- | Helper for the above
doubleFoldVariations' ::
  -- | The accumulated list of PlayerTeams
  [PlayerTeams] ->
  -- | The list of Variations we're working through
  [Variation] ->
  -- | The resultant list of PlayerTeams
  [PlayerTeams]
doubleFoldVariations' pts [] = pts
doubleFoldVariations' [] (Variation v : vs) = doubleFoldVariations' (map (\(p, t) -> (p, [t])) v) vs
doubleFoldVariations' pts (Variation v : vs) =
  let allCurrentNames = map fst pts -- Get all names
      allNewNames = filter (not . (`elem` allCurrentNames)) (map fst v) -- Get all names that we don't already have
      lengthSoFar = length . snd . head $ pts -- How many Variations have we gone through?
      newPTInit = pts ++ map (, replicate lengthSoFar NoTeam) allNewNames -- Add new names and appropriately lengthed lists of NoTeams
      newPT = map (doubleFold'' (Variation v)) newPTInit -- Add everything from the newest Variation
   in doubleFoldVariations' newPT vs -- And do the next

-- | Helper for the above
doubleFold'' ::
  -- | The Variation we're looking at
  Variation ->
  -- | The list of PlayerTeams we're investigating
  PlayerTeams ->
  -- | the resultant PlayerTeams
  PlayerTeams
doubleFold'' (Variation v) (p, ts) =
  case find ((== p) . fst) v of
    Nothing     -> (p, NoTeam : ts)
    Just (_, t) -> (p, t : ts)

-- | Sorting 2 Players based on their position in the initial squad
compareBasedOnSquad ::
  -- | The initial squad
  Lineup ->
  -- | The first Player
  Player ->
  -- | The second Player
  Player ->
  -- | The resultant Ordering
  Ordering
compareBasedOnSquad l p1 p2 =
  compare (compareBasedOnSquad' l p1) (compareBasedOnSquad' l p2)

-- | Getting the index for a single player
compareBasedOnSquad' :: Lineup -> Player -> Int
compareBasedOnSquad' l p = fromMaybe minBound (findIndex ((== p) . fst) l)
