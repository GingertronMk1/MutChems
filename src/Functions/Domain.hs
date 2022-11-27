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
import           Data.Squad
import qualified Data.Teams            as T
import           Functions.Application
import           Type

-- | Does a given `TeamOrMultiple` contain a given t`Type.Team`.
includesTeam ::
  -- | The t`Type.Team` being searched for.
  Team ->
  -- | The TeamOrMultiple being searched.
  TeamOrMultiple ->
  -- | Does it contain?.
  Bool
includesTeam t = elem t . expandTeamOrMultiple

-- | How many options do we get from a given `Lineup`?.
numberOfOptionsFn :: Lineup -> Int
numberOfOptionsFn = product . map (length . snd)

-- | Give me a list of all t`Type.Team` in a given Lineup.
allTeamsFn :: Lineup -> [Team]
allTeamsFn = concatMap expandTeamOrMultiple . concatMap snd

-- | The maximum total number of `Variation`s allowed across the whole calculation
maxTotalVariations :: Int
maxTotalVariations = 25 * 1000000

-- | The maximum number of `Variation`s allowed per squad
squadFilterThreshold :: Int
squadFilterThreshold = div maxTotalVariations (length prospectiveAdditions + 1)


-- * Filtering the squad to limit the number of possible options

-- | Filter a given squad such that it contains only `squadFilterThreshold` options
filteredSquadFn :: Lineup -> Lineup
filteredSquadFn = filteredSquadFn' 0

-- | Helper for the above - does the actual filtering
filteredSquadFn' ::
  -- | The threshold number - if there are fewer than this many instances of a
  -- t`Type.Team` in a Lineup we can disregard it
  Int ->
  -- | The initial lineup to be filtered
  Lineup ->
  -- | The resultant lineup
  Lineup
filteredSquadFn' threshold s =
  let allTeams = allTeamsFn s
      newS                = map (second $ filteredSquadFn'' $ filterFn threshold allTeams) s
      numberOfNewSOptions = numberOfOptionsFn newS
   in if 0 < numberOfNewSOptions && numberOfNewSOptions <= squadFilterThreshold
      then newS
      else filteredSquadFn' (threshold + 1) newS

-- | The function we use to filter the list of `TeamOrMultiple`s in the squad
filterFn ::
  -- | The threshold number - if there are fewer than this many instances of a
  -- t`Type.Team` in a Lineup we can disregard it
  Int ->
  -- | The list of t`Type.Team`s we should be comparing against
  [Team] ->
  -- | The `TeamOrMultiple` we're considering
  TeamOrMultiple ->
  -- | The resultant boolean value
  Bool
filterFn threshold ts tom = case tom of
    NoTeam             -> False
    (Team t)           -> filterFn' t
    (MultipleTeam t _) -> filterFn' t
    (Teams teams)      -> any (filterFn threshold ts) teams
    where filterFn' t = numberOfOneTeam t > threshold || t == T.all32Teams
          numberOfOneTeam t = length . filter (== t) $ ts

-- | A helper to be used in the mapping for the above
filteredSquadFn'' ::
  -- | Nominally the `filterFn` defined in the above's `let` block - should maybe pull that out
  -- into its own function
  (TeamOrMultiple -> Bool) ->
  -- | Input list of TeamOrMultiples
  [TeamOrMultiple] ->
  -- | Resultant list of TeamOrMultiples
  [TeamOrMultiple]
filteredSquadFn'' f ts = case filter f ts of
  [] -> [NoTeam]
  xs -> xs

-- * Converting those players who have the ability to have any team chemistry
-- applied to them

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
  -- | The list of Teams that should be considered for conversion.
  [Team] ->
  -- | The TeamOrMultiple being converted.
  TeamOrMultiple ->
  -- | The resultant list.
  [TeamOrMultiple]
convertSingle _ NoTeam = [NoTeam]
convertSingle ts team@(Team t) =
  if t == T.all32Teams
    then map Team ts
    else [team]
convertSingle ts (MultipleTeam t i) =
  if t == T.all32Teams
    then map (`MultipleTeam` i) ts
    else [MultipleTeam t i]
convertSingle ts (Teams t) = concatMap (convertSingle ts) t

-- * "Double folding" `Type.Variation`s - sort of rotating the 2D list of teams
-- and players such that we can represent it line-by-line, like in a MarkDown
-- file for instance?

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

-- * Sorting players in resultant `Type.Variation`s

-- | Sorting 2 Players based on their position in the initial squad.
compareBasedOnSquad ::
  -- | The initial squad.
  Lineup ->
  -- | The first Player.
  Player ->
  -- | The second Player.
  Player ->
  -- | The resultant Ordering.
  Ordering
compareBasedOnSquad l p1 p2 =
  compare (compareBasedOnSquad' l p1) (compareBasedOnSquad' l p2)

-- | Getting the index for a single player.
compareBasedOnSquad' :: Lineup -> Player -> Int
compareBasedOnSquad' l p = fromMaybe minBound (findIndex ((== p) . fst) l)

-- * Adding prospective players to a Lineup

-- | A function to combine a Lineup with a list of ProspectiveAdditions,
-- respecting the replacement/addition options
addProspectives ::
  -- | The list of ProspectiveAdditions
  [ProspectiveAddition] ->
  -- | The Lineup to which they are being added
  Lineup ->
  -- | The resultant Lineup
  Lineup
addProspectives pts l = foldl (flip addProspective) l pts

-- | Add a single prospective addition to the squad
addProspective :: ProspectiveAddition -> Lineup -> Lineup
addProspective NoChange l = l
addProspective (Addition pt) l = l ++ [pt]
addProspective (Replacement p pt) l =
  let (firstPart, theRest) = splitAtPredicate ((==p) . fst) l
   in firstPart ++ [pt] ++ theRest


-- | Add each ProspectiveAddition in turn to the squad, keeping the initial squad
addProspectivesInTurn :: [ProspectiveAddition] -> Lineup -> [(ProspectiveAddition, Lineup)]
addProspectivesInTurn ps l = (NoChange, l) : addProspectivesInTurn' ps l

-- | Add the remaining prospectives in turn
addProspectivesInTurn' :: [ProspectiveAddition] -> Lineup -> [(ProspectiveAddition, Lineup)]
addProspectivesInTurn' [] _ = []
addProspectivesInTurn' (p:ps) l =
  let newL = addProspective p l
   in (p, newL) : addProspectivesInTurn' ps newL

-- * Converting a `Type.Lineup` into something we can actually handle

-- | Turn a Lineup into one where all of the `Data.Teams.all32Teams` players have been given
-- their teams and filtered by team popularity
convertSquad :: Lineup -> Lineup
convertSquad = filteredSquadFn . convertAll32Teams

-- | Convert a Lineup to all of its top 10 variations
lineupToVariations :: Lineup -> [Variation]
lineupToVariations = take 10
                   . maximumValues
                   . map (Variation . sortOn snd)
                   . sequence
                   . expandList
