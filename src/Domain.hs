{-# LANGUAGE TupleSections #-}
-- |
-- Module: Type
module Domain where

import           Application
import           Data.Bifunctor
import           Data.List
import           Data.Maybe


-- | A useful shorthand for any player who can have all 32 teams.
all32Teams :: Domain.Team
all32Teams = "all32"

-- | The Chicago Bears
bears :: Domain.Team
bears = "Bears"

-- | The Cincinnati Bengals
bengals :: Domain.Team
bengals = "Bengals"

-- | The Buffalo Bills
bills :: Domain.Team
bills = "Bills"

-- | The Denver Broncos
broncos :: Domain.Team
broncos = "Broncos"

-- | The Cleveland Browns
browns :: Domain.Team
browns = "Browns"

-- | The Tampa Bay Buccaneers
buccaneers :: Domain.Team
buccaneers = "Buccaneers"

-- | The Arizona Cardinals
cardinals :: Domain.Team
cardinals = "Cardinals"

-- | The LA Chargers
chargers :: Domain.Team
chargers = "Chargers"

-- | The Kansas City Chiefs
chiefs :: Domain.Team
chiefs = "Chiefs"

-- | The Indianapolis Colts
colts :: Domain.Team
colts = "Colts"

-- | The Washington Commanders
commanders :: Domain.Team
commanders = "Commanders"

-- | The Dallas Cowboys
cowboys :: Domain.Team
cowboys = "Cowboys"

-- | The Miami Dolphins
dolphins :: Domain.Team
dolphins = "Dolphins"

-- | The Philadelphia Eagles
eagles :: Domain.Team
eagles = "Eagles"

-- | The Atlanta Falcons
falcons :: Domain.Team
falcons = "Falcons"

-- | The New York Giants
giants :: Domain.Team
giants = "Giants"

-- | The Jacksonville Jaguars
jaguars :: Domain.Team
jaguars = "Jaguars"

-- | The New York Jets
jets :: Domain.Team
jets = "Jets"

-- | Legends
legends :: Domain.Team
legends = "Legends"

-- | The Detroit Lions
lions :: Domain.Team
lions = "Lions"

-- | The San Francisco 49ers
niners :: Domain.Team
niners = "49ers"

-- | The Green Bay Packers
packers :: Domain.Team
packers = "Packers"

-- | The Carolina Panthers
panthers :: Domain.Team
panthers = "Panthers"

-- | The New England Patriots
patriots :: Domain.Team
patriots = "Patriots"

-- | The Las Vegas Raiders
raiders :: Domain.Team
raiders = "Raiders"

-- | The LA Rams
rams :: Domain.Team
rams = "Rams"

-- | The Baltimore Ravens
ravens :: Domain.Team
ravens = "Ravens"

-- | The New Orleans Saints
saints :: Domain.Team
saints = "Saints"

-- | The Seattle Seahawks
seahawks :: Domain.Team
seahawks = "Seahawks"

-- | The Pittsburgh Steelers
steelers :: Domain.Team
steelers = "Steelers"

-- | The Houston Texans
texans :: Domain.Team
texans = "Texans"

-- | The Tennessee Titans
titans :: Domain.Team
titans = "Titans"

-- | The Minnesota Vikings
vikings :: Domain.Team
vikings = "Vikings"


-- | Team is shorthand for a String - it is just the name of a team.
type Team = String

-- | Player is shorthand for a String - it is just the name of a football player.
type Player = String

-- | A player and all of their teams.
type PlayerTeams = (Player, [TeamOrMultiple])

-- | A full lineup.
type Lineup = [PlayerTeams]

-- | One variation I can have with a Lineup.
newtype Variation
  = Variation [(Player, TeamOrMultiple)]
  deriving (Eq, Show)

-- | A team and a list of all players with that team's chemistry.
type TeamPlayer = (Team, [Player])

-- | An option for the whole squad's chemistries.
type Option = [TeamPlayer]

-- | Options for one or more Teams.
data TeamOrMultiple
  -- | Null value.
  = NoTeam
  -- | A single Team.
  | Team Team
  -- | A single Team with a multiplier, e.g. Raiders x3.
  | MultipleTeam Team Int
  -- | Multiple Teams, e.g. Broncos + Seahawks.
  | Teams [TeamOrMultiple]
  deriving (Eq, Show)


-- | A type to represent potential additions/replacements for my squad
data ProspectiveChange
  -- | A Player who will replace another Player in the Lineup
  = Replacement Player PlayerTeams
  -- | A Player who will fit in without displacing another Player
  | Addition PlayerTeams
  -- | No addition or replacement
  | NoChange
  -- | Removing a player
  | Removal Player
  -- | Removing multiple players in one go
  | Removals [Player]
  deriving (Eq, Show)


-- My imports

-- Haskell imports
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
squadFilterThreshold = 5000000


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
      newS                = map (second . filteredSquadFn'' $ filterFn threshold allTeams) s
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
    where filterFn' t = numberOfOneTeam t > threshold || t == all32Teams
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
convertAll32Teams l = map (
                      second
                      . concatMap
                      . convertSingle
                      . rmDups
                      . filter (/= all32Teams)
                      . allTeamsFn
                      $ l
                      ) l

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
  if t == all32Teams
    then map Team ts
    else [team]
convertSingle ts (MultipleTeam t i) =
  if t == all32Teams
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

-- | A function to combine a Lineup with a list of ProspectiveChanges,
-- respecting the replacement/addition options
addProspectives ::
  -- | The list of ProspectiveChanges
  [ProspectiveChange] ->
  -- | The Lineup to which they are being added
  Lineup ->
  -- | The resultant Lineup
  Lineup
addProspectives pts l = foldl (flip addProspective) l pts

-- | Add a single prospective addition to the squad
addProspective :: ProspectiveChange -> Lineup -> Lineup
addProspective NoChange l = l
addProspective (Addition pt) l = l ++ [pt]
addProspective (Replacement p pt) l =
  let (firstPart, theRest) = splitAtPredicate ((==p) . fst) l
   in firstPart ++ [pt] ++ theRest
addProspective (Removal p) l = filter ((/=p) . fst) l
addProspective (Removals ps) l = filter (\(p,_) -> p `notElem` ps) l


-- | Add each ProspectiveChange in turn to the squad, keeping the initial squad
addProspectivesInTurn :: [ProspectiveChange] -> Lineup -> [(ProspectiveChange, Lineup)]
addProspectivesInTurn ps l = (NoChange, l) : addProspectivesInTurn' ps l

-- | Add the remaining prospectives in turn
addProspectivesInTurn' :: [ProspectiveChange] -> Lineup -> [(ProspectiveChange, Lineup)]
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


-- | The Ord instance - compare the "lowest" team name in each.
instance Ord TeamOrMultiple where
  -- We're going to base this on MultipleTeams
  -- Comparing 2 MultipleTeams
  compare (MultipleTeam t1 i1)  (MultipleTeam t2 i2) = case compare t1 t2 of
    EQ -> compare i1 i2   -- If it's the same Team then compare how many it is
    c  -> c               -- Otherwise compare the Team
  compare t1@(MultipleTeam _ _) (Team t2)             = compare t1 (MultipleTeam t2 1)
  compare t1@(MultipleTeam _ _) (Teams t2s)           = compare t1 (maximum t2s)
  compare (Team t1)             (Team t2)             = compare (MultipleTeam t1 1) (MultipleTeam t2 1)
  compare (Team t1)             t2@(MultipleTeam _ _) = compare (MultipleTeam t1 1) t2
  compare (Team t1)             (Teams t2s)           = compare (MultipleTeam t1 1) (maximum t2s)
  compare (Teams t1s)           t2@(Team _)           = compare (maximum t1s) t2
  compare (Teams t1s)           t2@(MultipleTeam _ _) = compare (maximum t1s) t2
  compare (Teams t1s)           (Teams t2s)           = compare (maximum t1s) (maximum t2s)
  compare NoTeam                _                     = LT
  compare _                     NoTeam                = GT





-- | Expanding a TeamOrMultiple into a list of Teams - used for analysis.
expandTeamOrMultiple :: TeamOrMultiple -> [Team]
expandTeamOrMultiple NoTeam             = []
expandTeamOrMultiple (Team t)           = [t]
expandTeamOrMultiple (MultipleTeam t i) = replicate i t
expandTeamOrMultiple (Teams ts)         = concatMap expandTeamOrMultiple ts

runThroughPreferences :: [Team] -> [(Team, Int)] -> [(Team, Int)] -> Ordering
runThroughPreferences [] _ _ = EQ
runThroughPreferences (p:ps) v1 v2 = case compare (numTeam v2) (numTeam v1) of
  EQ -> runThroughPreferences ps v1 v2
  c  -> c
  where numTeam tns = case find (\(t,_) -> t == p) tns of
          Just (_,n) -> n
          Nothing    -> 0
