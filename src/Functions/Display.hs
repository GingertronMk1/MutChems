-- | Module: Functions.Display
module Functions.Display where

import           Data.Calculated
import           Data.List
import           Data.Ord
import           Functions.Application
import           Functions.Domain
import           Type

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam             = "-"
ppTeamOrMultiple (Team t)           = t
ppTeamOrMultiple (MultipleTeam t i) = t ++ "x" ++ show i
ppTeamOrMultiple (Teams ts)         = intercalate "/" $ map show ts

-- | Prettily print a Variation
ppVariation :: Variation -> String
ppVariation (Variation vs) =
  intercalate "\n"
    . map ppVariation'
    . sortBy (\(p1,_) (p2,_) -> compareBasedOnSquad squad p1 p2)
    $ vs
  where
    ppVariation' (p, t) = p ++ ": " ++ ppTeamOrMultiple t

-- | Prettily print many variations, separated cleanly
ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation

-- | Prettily print some double-folded variations to a nice Markdown string
genMarkdown ::
  -- | A list of (Player, [TeamOrMultiple]) tuples
  [PlayerTeams] ->
  -- | A markdown table
  String
genMarkdown dfvs =
  let sortedDfvs = sortBy (\(p1,_) (p2,_) -> compareBasedOnSquad squad p1 p2) dfvs
      totalCols = length . snd . head $ sortedDfvs
      longestPlayerNameLength = maximum . map (length . fst) $ sortedDfvs
      longestTeamNameLength = maximum . concatMap (map (length . ppTeamOrMultiple) . snd) $ sortedDfvs
      longestPlayerNameLengthPlus4 = longestPlayerNameLength + 4
      longestTeamNameLengthPlus4 = longestTeamNameLength + 4
      topRow =
        printf
          "| %s | %s |"
          [ padRight longestPlayerNameLengthPlus4 ' ' "Player",
            intercalate " | " . map (padRight longestTeamNameLengthPlus4 ' ' . show) $ [1 .. totalCols]
          ]
      secondRow =
        printf
          "|%s|%s"
          [ replicate (longestPlayerNameLengthPlus4 + 2) '-',
            concat (replicate totalCols (printf ":%s:|" [replicate longestTeamNameLengthPlus4 '-']))
          ]
      theRest =
        intercalate "\n"
          . map (genMarkdown' longestPlayerNameLengthPlus4 longestTeamNameLengthPlus4)
          $ sortedDfvs
      bottomRow = printf "| **TOTALS** | %s |" [totalsPerSquad sortedDfvs]
   in intercalate
        "\n"
        [ topRow,
          secondRow,
          theRest,
          bottomRow
        ]

-- | Helper for the above - make a Markdown table row for a single PlayerTeam
genMarkdown' :: Int -> Int -> PlayerTeams -> String
genMarkdown' lp lt (p, ts) =
  printf
    "| %s | %s |"
    [ padRight lp ' ' (printf "**%s**" [p]),
      (intercalate " | " . map (padRight lt ' ' . ppTeamOrMultiple)) ts
    ]

-- | Using the totals of each team in each Variation, kind of unfolding them?
totalsPerSquad :: [PlayerTeams] -> String
totalsPerSquad pts =
  let rotated = rotate . map snd $ pts
      amounts = map (sortOn (Down . snd) . map (\ts -> (head ts, length ts)) . group . sort) rotated
  in intercalate "|" . map (intercalate "<br>" . map (\(t,i) -> printf "%s: %s" [ppTeamOrMultiple t, show i])) $ amounts
