{-|
Module: Functions

-}
module Functions where

import Data
import Data.Bifunctor as DB
import Data.List
import Data.Ord
import Type


{- Utility functions -}
-- | Remove duplicate items from a list
rmDups :: (Eq a, Ord a) => [a] -> [a]
rmDups = map head . group . sort

-- | Pad a string right with a given char until it is of a certain length
padRight :: Int -> Char -> String -> String
padRight l padding str = str ++ replicate (l - length str) padding

-- | Take the mean of a list of Integral values
mean :: (Integral a) => [a] -> Float
mean ls = fromIntegral (length ls) / fromIntegral (sum ls)

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

-- | Take the minimum distance from a multiple of 5 that a number is
distanceFrom5 :: Int -> Int
distanceFrom5 n = (\v -> min v (5 - v)) $ mod n 5

-- | Take the average distance from a multiple of 5 that a list of numbers are
avgDistanceFromMultiplesOf5 :: [Int] -> Float
avgDistanceFromMultiplesOf5 = mean . map distanceFrom5


{- Domain -}

-- | Take the most best option for a team captain
bestCaptainOption :: Option -> Option
bestCaptainOption = last . bestCaptainOption'

-- | Generate a list of options for the team captain, and order them from "worst" to "best"
bestCaptainOption' :: Option -> [Option]
bestCaptainOption' o =
  if captainTeam `elem` map fst o
    then
      let captainName = head . snd . head $ filter (\(t, _) -> t == captainTeam) o
          remaining = filter (\(t, _) -> t /= captainTeam) o
       in sortBy orderOptions . map (sortOn (Down . length . snd)) . iterativeApplicationFn (DB.second (captainName :)) $ remaining
    else [o]

-- | Apply a function to each item in a list, returning a list of lists containing
-- | the result of each application
iterativeApplicationFn :: (Eq a) => (a -> a) -> [a] -> [[a]]
iterativeApplicationFn _ [] = []
iterativeApplicationFn f xs = iterativeApplicationFn' f xs xs

-- | Helper function for iterativeApplicationFn
iterativeApplicationFn' :: Eq a => (a -> a) -> [a] -> [a] -> [[a]]
iterativeApplicationFn' _ [] cs = [cs]
iterativeApplicationFn' _ _ [] = []
iterativeApplicationFn' fn bs (c : cs) =
  let bs' = filter (/= c) bs
   in (fn c : bs') : iterativeApplicationFn' fn bs' cs

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
  bestCaptainOption
    . sortOn (Down . length . snd)
    . map
      ( (\(ps, t : _) -> (t, ps))
          . unzip
      )
    . groupBy (\(_, t1) (_, t2) -> t1 == t2)
    . sortOn snd

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

-- | Sort a lineup by popularity (frequency) of teams
popularitySort :: Lineup -> Lineup
popularitySort l = map (DB.second $ sortBy $ popSort' l) l
  where popSort' l' t1 t2 = compare (popSort'' t2 l') (popSort'' t1 l')
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
      numOfOneTeam t = length . filter (t==) $ allTeams
      filterTeamList = filter (\t -> numOfOneTeam t > 4 || t == captainTeam)
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
  where foldFunction' [] _ = []
        foldFunction' (o : os) biggestO =
          if orderOptions o biggestO == GT
          then o : foldFunction' os o
          else foldFunction' os biggestO

{- Useful for debugging/testing -}

-- | Nicely print a Lineup
ppSquad :: Lineup -> IO()
ppSquad = 
  putStrLn
  . intercalate "\n"
  . map (\(p, ts) -> p ++ ": " ++ intercalate ", " ts)