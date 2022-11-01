-- | Module: Functions.Display
module Functions.Display where

import Functions.Application
import Type
import Data.List

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam = "-"
ppTeamOrMultiple (Team t) = t
ppTeamOrMultiple (MultipleTeam t i) = t ++ "x" ++ show i
ppTeamOrMultiple (Teams ts) = intercalate "/" $ map show ts

-- | Prettily print a Variation
ppVariation :: Variation -> String
ppVariation (Variation vs) =
  intercalate "\n"
  . map ppVariation'2
  . sortBy ppVariation'1
  $ vs
  where ppVariation'1 (p1, t1) (p2, t2) =
          case compare t1 t2 of
            EQ -> compare p1 p2
            c -> c
        ppVariation'2 (p, t) = p ++ ": " ++ ppTeamOrMultiple t


-- | Prettily print many variations, separated cleanly
ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation

-- | Prettily print some double-folded variations to a nice Markdown string
genMarkdown :: [PlayerTeams] -- ^ A list of (Player, [TeamOrMultiple]) tuples
                         -> String        -- ^ A markdown table
genMarkdown dfvs =
  let totalCols = length . snd . head $ dfvs
      longestPlayerNameLength = maximum . map (length . fst) $ dfvs
      longestTeamNameLength = maximum . concatMap (map (length . ppTeamOrMultiple) . snd) $ dfvs
      longestPlayerNameLengthPlus4 = longestPlayerNameLength + 4
      longestTeamNameLengthPlus4 = longestTeamNameLength + 4
      topRow = printf
        "| %s | %s |"
        [ padRight longestPlayerNameLengthPlus4 ' ' "Player",
          intercalate " | " . map (padRight longestTeamNameLengthPlus4 ' ' . show) $ [1..totalCols]
        ]
      secondRow = printf
        "|%s|%s"
        [ replicate (longestPlayerNameLengthPlus4 + 2) '-',
          concat (replicate totalCols (printf ":%s:|" [replicate longestTeamNameLengthPlus4 '-']))
        ]
   in intercalate "\n" [
      topRow,
      secondRow,
      intercalate "\n" . map (genMarkdown' longestPlayerNameLengthPlus4 longestTeamNameLengthPlus4) $ dfvs
   ]

-- | Helper for the above - make a Markdown table row for a single PlayerTeam
genMarkdown' :: Int -> Int ->  PlayerTeams -> String
genMarkdown' lp lt (p, ts) = printf "| %s | %s |" [padRight lp ' ' (printf "**%s**" [p]), (intercalate " | " . map (padRight lt ' ' . ppTeamOrMultiple)) ts]