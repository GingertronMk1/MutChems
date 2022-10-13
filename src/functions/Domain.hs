-- |
-- Module: Functions.Domain
module Functions.Domain where

import Functions.Application
import Data
import Data.Bifunctor as DB
import Data.List
import Data.Ord
import Type

-- * More specific (Domain) functions

-- | Returns whether tps2 should be higher up the list than tps1
orderOptions :: Option -> Option -> Ordering
orderOptions o1 o2 =
  let lengths = map (length . snd) . take 3
   in fst $ orderOptions' (lengths o1) (lengths o2)

-- | Helper function for orderOptions
orderOptions' :: [Int] -> [Int] -> (Ordering, String)
orderOptions' xs ys
  | sumComp /= EQ = (sumComp, "Sum")
  | numComp /= EQ = (numComp, "5s")
  | otherwise = (distComp, "Dist")
  where
    sumComp = compare (sum xs) (sum ys)
    num5s = length . filter (== 0) . map (`mod` 5)
    numComp = compare (num5s xs) (num5s ys)
    distComp = compare (avgDistanceFromMultiplesOf5 ys) (avgDistanceFromMultiplesOf5 xs)

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

-- | Get a list of all Teams in a lineup
allTeamsFn :: Lineup -> [Team]
allTeamsFn = rmDups . concatMap snd

-- | Filter a Lineup such that it contains only teams that could be a viable Option
popFilter :: Lineup -> Lineup
popFilter l =
  let allTeams = concatMap snd l
      numOfOneTeam t = length . filter (t ==) $ allTeams
      filterTeamList = filter (\t -> numOfOneTeam t > 4 || t == all32Teams)
   in filter (not . null . snd) . map (DB.second filterTeamList) $ l

-- | Convert a Lineup to a list of lists of Players and Teams
lineupToPlayerTeams :: Lineup -> [[(Player, Team)]]
lineupToPlayerTeams = mapM (\(p, ts) -> [(p, t) | t <- ts])

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

-- | Nicely print a Lineup
ppSquad :: Lineup -> IO ()
ppSquad =
  putStrLn
    . intercalate "\n"
    . map (\(p, ts) -> p ++ ": " ++ intercalate ", " ts)

-- | Nicely print an Option
ppOption :: Option -> String
ppOption o =
  let longestTeamNameLength = length . maximumBy (comparing length) . map fst $ o
      largestNumber = length . show . maximum . map (length . snd) $ o
      indent = replicate 2 ' '
   in intercalate "\n"
        . map
          ( \(team, p@(player : players)) ->
              indent
                ++ padRight longestTeamNameLength ' ' team
                ++ " | "
                ++ padRight largestNumber ' ' ((show . length) p)
                ++ " | "
                ++ player
                ++ concatMap
                  ( \player' ->
                      "\n"
                        ++ indent
                        ++ replicate longestTeamNameLength ' '
                        ++ " | "
                        ++ replicate largestNumber ' '
                        ++ " | "
                        ++ player'
                  )
                  players
          )
        $ o

-- | Nicely print a list of Options
ppOptions :: [Option] -> String
ppOptions = intercalate "\n\n" . map ppOption

-- | Print a nicely formatted list of Options
putPPOptions :: [Option] -> IO ()
putPPOptions = putStrLn . ppOptions

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