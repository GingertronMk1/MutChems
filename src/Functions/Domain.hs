-- |
-- Module: Functions.Domain
--
-- Domain functions, i.e. those which are more specific to this project and the
-- data structures it contains
module Functions.Domain where

import Data
import Data.Bifunctor as DB
import Data.List
import Data.Ord
import Functions.Application
import Type

-- * More specific (Domain) functions

-- | Returns whether tps2 should be higher up the list than tps1
orderOptions :: Option -> Option -> Ordering
orderOptions o1 o2 =
  let lengths = map (length . convertMultiples . snd) . take 3
   in fst $ orderListOfInts (lengths o1) (lengths o2)

-- | Convert a list of players and teams to an Option
playerTeamToOption :: [(Player, Team)] -> Option
playerTeamToOption =
  sortOn (Down . length . snd)
    . map
      ( (\(ps, t : _) -> (t, ps))
          . unzip
      )
    . groupBy (\(_, t1) (_, t2) -> t1 == t2)
    . sortOn snd

-- | Sort a lineup by popularity (frequency) of teams
popularitySort :: Lineup -> Lineup
popularitySort l = map (DB.second $ sortBy $ popSort' l) l
  where
    popSort' l' t1 t2 = compare (popSort'' t2 l') (popSort'' t1 l')
    popSort'' t = length . filter (== t) . concatMap snd

-- | How many Options will a given Lineup generate
numOptionsFn :: Lineup -> Int
numOptionsFn = product . map (length . snd)

-- | Get a list of all unique Teams in a Lineup
allTeamsFn :: Lineup -> [Team]
allTeamsFn = rmDups . allTeamsWithNumbers

-- | Get a list of all Teams in a Lineup
allTeamsWithNumbers :: Lineup -> [Team]
allTeamsWithNumbers = concatMap (convertMultiples . snd)

-- | Filter a Lineup such that it contains only teams that could be a viable Option
popFilter :: Lineup -> Lineup
popFilter l =
  let allTeams = allTeamsWithNumbers l
      numOfOneTeam t = length . filter (t ==) $ allTeams
      filterTeamList = filter (\t -> (numOfOneTeam . fst . breakStringWithNumber $ t) > 4 || t == all32Teams)
   in filter (not . null . snd) . map (DB.second filterTeamList) $ l

-- | Convert a Lineup to a list of lists of Players and Teams
lineupToPlayerTeams :: Lineup -> [[(Player, Team)]]
lineupToPlayerTeams = mapM (\(p, ts) -> lineupToPlayerTeams' p ts)

-- | Helper for lineupToPlayerTeams
lineupToPlayerTeams' :: Player -> [Team] -> [(Player, Team)]
lineupToPlayerTeams' p = map (lineupToPlayerTeams'' p)

-- | Adding the number of a given team chemistry to the player's name
lineupToPlayerTeams'' :: Player -> Team -> (Player, Team)
lineupToPlayerTeams'' p t =
  let (t', tn) = break (=='|') t 
   in (p ++ tn, t')

-- | The function to fold a list of options down into the best one
-- | It generates a list such that it can be printed and give some idea
-- | Of progress
foldFunction :: [Option] -> [Option]
foldFunction [] = []
foldFunction options = foldFunction' options []
  where
    foldFunction' [] _ = []
    foldFunction' (o : os) biggestO =
      if orderOptions o biggestO == GT
        then o : foldFunction' os o
        else foldFunction' os biggestO

-- | Takes a lineup and converts any players who can have all 32 team chems
-- into players with all team chemistries in the lineup
convert32TeamPlayers :: Lineup -> Lineup
convert32TeamPlayers l =
  map
    ( convert32TeamPlayer
        ( filter (/= all32Teams)
            . rmDups
            . concatMap snd
            $ l
        )
    )
    l

-- | Take a single player, and if they can have all 32 team chemistries, give him
-- the chemistries of all teams provided
convert32TeamPlayer :: [Team] -> PlayerTeams -> PlayerTeams
convert32TeamPlayer ts (p, pts) =
  if all32Teams `elem` pts
    then (p, ts)
    else (p, pts)

-- * Prettily printing values

-- | Comma-separate a number for human reading
makeNumberHumanReadable :: Int -> String
makeNumberHumanReadable =
  reverse
    . intercalate ","
    . makeNumberHumanReadable'
    . reverse
    . show
  where
    makeNumberHumanReadable' [] = []
    makeNumberHumanReadable' xs =
      let (firstNum, restNums) = splitAt 3 xs
       in firstNum : makeNumberHumanReadable' restNums

-- | Give me the number of each team chemistry in the lineup
numOfEachTeam :: Lineup -> [(Team, Int)]
numOfEachTeam =
  sortOn (Down . snd)
    . map (\g -> (head g, length g))
    . group
    . sort
    . concatMap snd

-- | Helper for calculatedData such that I can more easily see the effect of the
-- popFilter function
processSquad :: Lineup -> Lineup
processSquad = popularitySort . convert32TeamPlayers