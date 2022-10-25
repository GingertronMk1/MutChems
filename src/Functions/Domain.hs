-- |
-- Module: Functions.Domain
--
-- Domain functions, i.e. those which are more specific to this project and the
-- data structures it contains
module Functions.Domain where

-- My imports
import qualified Data.Teams as Teams
import Functions.Application
import Type

-- Haskell imports
import Data.Bifunctor
import Data.List


-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam = []
expandTeamOrMultiple (Team t) = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts) = concatMap expandTeamOrMultiple ts

-- | does a given TeamOrMultiple contain a given Team
includesTeam :: Team            -- ^ The Team being searched for
             -> TeamOrMultiple  -- ^ The TeamOrMultiple being searched
             -> Bool            -- ^ Does it contain?
includesTeam t = elem t . expandTeamOrMultiple


-- | How many options do we get from a given Lineup?
numberOfOptionsFn :: Lineup -> Int
numberOfOptionsFn = product . map (length . snd)

-- | Give me a list of all Teams in a given Lineup
allTeamsFn :: Lineup -> [Team]
allTeamsFn = concatMap expandTeamOrMultiple
           . concatMap snd


-- | Filtering a Lineup to contain only those Teams with 3 or more entries
filteredSquadFn :: Lineup -> Lineup
filteredSquadFn s =
  let allTeams = allTeamsFn s
      numberOfOneTeam t = length . filter (==t) $ allTeams
      filterFn' t = numberOfOneTeam t > 3 || t == Teams.all32Teams
      filterFn NoTeam = False
      filterFn (Team t) = filterFn' t
      filterFn (MultipleTeam t _) = filterFn' t
      filterFn (Teams ts) = any filterFn ts
   in filter (not . null . snd) . map (Data.Bifunctor.second (filter filterFn)) $ s

-- | Change all players with all 32 teams to contain all useful teams
-- useful here being "all other actual teams" - there's no point giving him
-- the option for Jacksonvill if nobody else has ever played for them
convertAll32Teams :: Lineup -> Lineup
convertAll32Teams l =
  let allTeams = rmDups
               . filter (/= Teams.all32Teams)
               . allTeamsFn
               $ l
    in map (second $ concatMap $ convertSingle allTeams) l


-- | Convert a single TeamOrMultiple to a list of Teams should that TeamOrMultiple
-- be all 32 teams
convertSingle :: [Team] -> TeamOrMultiple -> [TeamOrMultiple]
convertSingle _ NoTeam = []
convertSingle ts team@(Team t) =
  if t == Teams.all32Teams
  then map Team ts
  else [team]
convertSingle ts (MultipleTeam t i) =
  if t == Teams.all32Teams
  then map (`MultipleTeam` i) ts
  else [MultipleTeam t i]
convertSingle ts (Teams t) = concatMap (convertSingle ts) t

-- | Ostensibly `compare` but just because I can't have an Ord instance for
-- Variation
orderVariations :: Variation -> Variation -> Ordering
orderVariations v1 v2 = fst $ orderVariations' v1 v2

-- | Helper function for the above, technically unnecessary but useful for debugging
orderVariations' :: Variation           -- ^ v1, the first Variation
                 -> Variation           -- ^ v2, the second Variation
                 -> (Ordering, String)  -- ^ the comparison of v1 against v2
orderVariations' v1 v2 =
  let convertFn = map length . group . sort . concatMap (expandTeamOrMultiple . snd)
      expandedV1 = convertFn v1
      expandedV2 = convertFn v2
   in orderListOfInts expandedV1 expandedV2

doubleFoldVariations :: [Variation] -> [PlayerTeams]
doubleFoldVariations = doubleFoldVariations' []

doubleFoldVariations' :: [PlayerTeams] -> [Variation] -> [PlayerTeams]
doubleFoldVariations' pts [] = pts
doubleFoldVariations' [] (v:vs) = doubleFoldVariations' (map (\(p, t) -> (p, [t])) v) vs
doubleFoldVariations' pts (v:vs) =
  -- First get all the names in
  -- Then make sure they've all got the right number of spaces
  let allCurrentNames = map fst pts
      allNewNames = filter (not . (`elem` allCurrentNames)) (map fst v)
      lengthSoFar = length . snd . head $ pts
      newPTInit = pts ++ map (\n -> (n, replicate lengthSoFar (NoTeam))) allNewNames
      newPT = map (doubleFold'' v) newPTInit
   in doubleFoldVariations' newPT vs

doubleFold'' :: Variation -> PlayerTeams -> PlayerTeams
doubleFold'' v (p, ts) =
  case (find (\(p', _) -> p' == p) v) of
    Nothing      -> (p, (NoTeam):ts)
    Just (_, t)  -> (p, t:ts)
