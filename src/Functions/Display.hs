-- | Module: Functions.Display
module Functions.Display where

import Type
import Functions.Application

import Data.List
import Data.Ord

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
      largestNumber = length . show . maximum . map (length . convertMultiples . snd) $ o
      indent = replicate 2 ' '
   in intercalate "\n"
        . map
          ( \(team, p@(player : players)) ->
              indent
                ++ padRight longestTeamNameLength ' ' team
                ++ " | "
                ++ padRight largestNumber ' ' ((show . length . convertMultiples) p)
                ++ " | "
                ++ breakAndPPString player
                ++ concatMap
                  ( \player' ->
                      "\n"
                        ++ indent
                        ++ replicate longestTeamNameLength ' '
                        ++ " | "
                        ++ replicate largestNumber ' '
                        ++ " | "
                        ++ breakAndPPString player'
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

-- | Take a String and Int tuple and print it nicely
ppBrokenString :: (String, Int) -> String
ppBrokenString (p, 1) = p
ppBrokenString (p, n) = p  ++ " (x" ++ show n ++ ")"

-- | Take a pipe-separated string and print it nicely
breakAndPPString :: String -> String
breakAndPPString = ppBrokenString . breakStringWithNumber

-- | Pretty print the number of each team
ppNumOfEachTeamFn :: [(Team, Int)] -> IO()
ppNumOfEachTeamFn tis =
  let longestTeamNameLength = maximum . map (length . fst) $ tis
  in putStrLn
   . intercalate "\n"
   . map (\(t, n) -> padRight (longestTeamNameLength+2)  ' ' (t ++ ": ") ++ show n)
   $ tis
