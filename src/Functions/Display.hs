-- | Module: Functions.Display.
module Functions.Display where

import           Data.Char
import           Data.List
import           Functions.Application
import           Types.Basic
import           Types.ProspectiveChange
import           Types.TeamOrMultiple
import           Types.Variation

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer.
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam             = "-"
ppTeamOrMultiple (Team t)           = t
ppTeamOrMultiple (MultipleTeam t i) = printf "%s x%s" [t, show i]
ppTeamOrMultiple (Teams ts)         = intercalate "/" $ map show ts

-- | Nicely print a Prospective Change
ppProspectiveChange :: ProspectiveChange -> String
ppProspectiveChange NoChange = "No change"
ppProspectiveChange (Addition (p, _, _)) = printf "Adding %s" [p]
ppProspectiveChange (Replacement p1 (p2, _)) = printf "Replacing %s with %s" [p1, p2]
ppProspectiveChange (Removal p) = printf "Getting rid of %s" [p]

-- | Print a Variation as a Html table
htmlTablePrintVariation :: Variation -> String
htmlTablePrintVariation (Variation v) =
  intercalate "\n" [
    "<table>",
    "<thead>",
    "<tr>",
    surroundInTag "th" "Player",
    surroundInTag "th" "Chemistry",
    "</tr>",
    "</thead>",
    surroundInTag "tbody" $ htmlTablePrintVariation' v,
    "<tfoot>",
    "<tr>",
    surroundInTag "td" "TOTALS",
    surroundInTag "td"
      . surroundInTag "ul"
      . intercalate "\n"
      . map (\(t,i) -> unBreakSpaces $ printf"<li>%s: %s</li>" [t, show i])
      . totalsPerSquad
      $ v,
    "</tr>",
    "</tfoot>",
    "</table>"
  ]

-- | Print the internals of a Variation
htmlTablePrintVariation' :: [(Player, TeamOrMultiple, Position)] -> String
htmlTablePrintVariation' = intercalate "\n" . htmlTablePrintVariation'' "none"

-- | Print each row of the table, breaking on position changes
htmlTablePrintVariation'' :: String -> [(Player, TeamOrMultiple, Position)] -> [String]
htmlTablePrintVariation'' _ [] = []
htmlTablePrintVariation'' oldPos ((player, team, position):ps) =
  let thisLine = printf "<tr><td>%s</td><td>%s</td></tr>" $ map unBreakSpaces [player, ppTeamOrMultiple team]
   in if position == oldPos
      then thisLine : htmlTablePrintVariation'' oldPos ps
      else printf "<tr><td colspan=2><b>%s</b></td></tr>" [map toUpper position] : thisLine : htmlTablePrintVariation'' position ps


-- | Generate Html for a set of ProspectiveChanges and Variations
genHtml :: [(ProspectiveChange, Lineup, Variation)] -> String
genHtml plvs =
  let tableHead = newLineMap (surroundInTag "th" . unBreakSpaces . ppProspectiveChange  . getFirst) plvs
      tableBody = concatMap (\(_,_,v) -> "<td style=\"vertical-align:top\">\n\n" ++ htmlTablePrintVariation v ++ "\n\n</td>") plvs
   in intercalate "\n" [
    "<table>",
    surroundInTag "tr" tableHead,
    surroundInTag "tr" tableBody,
    "</table>"
   ]

-- | Pop a string in tags
surroundInTag :: String -> String -> String
surroundInTag tag content = concat ["<", tag, ">", content, "</", tag, ">"]
