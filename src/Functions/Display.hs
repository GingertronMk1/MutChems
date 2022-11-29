-- | Module: Functions.Display.
module Functions.Display where

import           Functions.Application
import           Data.List
import           Data.Ord
import Types.ProspectiveChange
import Types.TeamOrMultiple
import Types.Variation

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer.
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam             = "-"
ppTeamOrMultiple (Team t)           = t
ppTeamOrMultiple (MultipleTeam t i) = printf "%s x%s" [t, show i]
ppTeamOrMultiple (Teams ts)         = intercalate "/" $ map show ts

-- | Prettily print some double-folded variations to a nice Markdown string.
genMarkdown ::
  -- | The lineup from which this markdown is initially generated
  Lineup ->
  -- | A list of (Player, [TeamOrMultiple]) tuples.
  [PlayerTeams] ->
  -- | A markdown table.
  String
genMarkdown s dfvs =
  let sortedDfvs = sortBy (\(p1,_) (p2,_) -> compareBasedOnSquad s p1 p2) dfvs
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
     . map (\(t,i) -> printf "%s:&nbsp;%s" [t, show i])
     . sortOn (Down . snd)
     . map firstAndLength
     . group
     . sort
     . concatMap expandTeamOrMultiple
   )
   . rotate
   . map snd

-- | Intercalate strings with markdown separators
intercalation :: (a -> String) -> [a] -> String
intercalation f = intercalate "\n\n---\n\n" . map f

-- | Take a Lineup and convert it to a markdowned set of Variations
squadToPrintedVariation :: Lineup -> String
squadToPrintedVariation l = genMarkdown l
                          . doubleFoldVariations
                          . lineupToVariations
                          . convertSquad
                          $ l

-- | Print all of the generated `Type.Lineup`s
printLineups :: [(ProspectiveChange, Lineup)] -> [String]
printLineups = map printLineupWithChange

-- | Print an individual `Type.Lineup` including the change made to create it
printLineupWithChange :: (ProspectiveChange, Lineup) -> String
printLineupWithChange (pa, l) =
  let topRow = case pa of
          NoChange               -> "# No change"
          Addition (p, _)        -> printf "# Adding %s" [p]
          Replacement p1 (p2, _) -> printf "# Replacing %s with %s" [p1, p2]
          Removal p              -> printf "# Getting rid of %s" [p]
          Removals ps            -> printf "# Getting rid of %s" [printListWithAnd ps]
  in intercalate
    "\n\n"
    [
      topRow,
      printf "### Checked %s Variations" [ppNumber (numberOfOptionsFn l)],
      squadToPrintedVariation l
    ]

-- | Print an integer number with commas as thousands separators
ppNumber :: Integral a => a -> String
ppNumber = reverse . ppNumber' . reverse . show . toInteger

-- | Helper function for the above
ppNumber' :: String -> String
ppNumber' n@[_,_,_]  = n
ppNumber' (x:y:z:ns) = (x:y:z:",") ++ ppNumber' ns
ppNumber' ns         = ns

printListWithAnd :: [String] -> String
printListWithAnd [s] = s
printListWithAnd ss@[_,_] = printf "%s and %s" ss
printListWithAnd ss = printf "%s, and %s" [intercalate ", " (init ss), last ss]
