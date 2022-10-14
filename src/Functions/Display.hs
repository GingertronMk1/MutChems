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
