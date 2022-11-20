-- | Module: Functions.Display.
module Functions.Display where

import           Data.Calculated
import           Data.List
import           Data.Ord
import           Functions.Application
import           Functions.Domain
import           Type

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer.
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam             = "-"
ppTeamOrMultiple (Team t)           = t
ppTeamOrMultiple (MultipleTeam t i) = t ++ "x" ++ show i
ppTeamOrMultiple (Teams ts)         = intercalate "/" $ map show ts

-- | Prettily print a Variation.
ppVariation :: Variation -> String
ppVariation (Variation vs) =
  intercalate "\n"
    . map ppVariation'
    . sortBy (\(p1,_) (p2,_) -> compareBasedOnSquad squad p1 p2)
    $ vs
  where
    ppVariation' (p, t) = p ++ ": " ++ ppTeamOrMultiple t

-- | Prettily print many variations, separated cleanly.
ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation

-- | Prettily print some double-folded variations to a nice Markdown string.
genMarkdown ::
  -- | A list of (Player, [TeamOrMultiple]) tuples.
  [PlayerTeams] ->
  -- | A markdown table.
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
          [ "Player",
            intercalate " | " . map show $ [1 .. totalCols]
          ]
      secondRow =
        printf
          "|%s|%s"
          [ replicate (longestPlayerNameLengthPlus4 + 2) '-',
            concat (replicate totalCols (printf ":%s:|" [replicate longestTeamNameLengthPlus4 '-']))
          ]
      theRest = intercalate "\n" . map genMarkdown' $ sortedDfvs
      bottomRow = printf "| **TOTALS** | %s |" [totalsPerSquad sortedDfvs]
   in intercalate
        "\n"
        [ topRow,
          secondRow,
          theRest,
          bottomRow
        ]

-- | Helper for the above - make a Markdown table row for a single PlayerTeam.
genMarkdown' :: PlayerTeams -> String
genMarkdown' (p, ts) =
  printf
    "| %s | %s |"
    [ printf "**%s**" [intercalate "&nbsp;" . words $ p],
      (intercalate " | " . map ppTeamOrMultiple) ts
    ]

-- | Using the totals of each team in each Variation, kind of unfolding them?.
totalsPerSquad :: [PlayerTeams] -> String
totalsPerSquad =
  intercalate "|"
   . map (
     intercalate "<br>"
     . map (\(t,i) -> printf "%s:&nbsp;%s" [ppTeamOrMultiple t, show i])
     . sortOn (Down . snd)
     . filter ((/= NoTeam) . fst)
     . map firstAndLength
     . group
     . sort
   )
   . rotate
   . map snd

-- | Intercalate strings with markdown separators
intercalation :: (a -> String) -> [a] -> String
intercalation f = intercalate "\n\n---\n\n" . map f

squadToPrintedVariation :: Lineup -> String
squadToPrintedVariation = genMarkdown
                        . doubleFoldVariations
                        . lineupToVariations
                        . convertSquad

addProspectiveAndPrint :: [ProspectiveAddition] -> Lineup -> [String]
addProspectiveAndPrint pas l =
  let firstString = "# No Additions\n\n" ++ squadToPrintedVariation l
  in firstString : addProspectiveAndPrint' pas l

addProspectiveAndPrint' :: [ProspectiveAddition] -> Lineup -> [String]
addProspectiveAndPrint' [] _ = []
addProspectiveAndPrint' (pa:pas) l = case pa of
  Addition (p, _) ->  printf "# Adding %s:\n\n%s" [p, printedNewL] : nextIteration
  Replacement p1 (p2, _) ->  printf "# Replacing %s with %s:\n\n%s" [p1, p2, printedNewL] : nextIteration
  where newL = addProspective pa l
        printedNewL = squadToPrintedVariation newL
        nextIteration = addProspectiveAndPrint' pas newL
