-- | Module: Functions.Display.
module Functions.Display where

import           Data.List
import           Functions.Application
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
ppProspectiveChange (Removals ps) = printf "Getting rid of %s" [printListWithAnd ps]

-- | Print a Variation as a MarkDown table
markDownTablePrintVariation :: Variation -> String
markDownTablePrintVariation (Variation v) =
  intercalate "\n" [
    "| Player | Chemistry |",
    "|---|---|",
    newLineMap (\(p,tom,_) -> printf "| %s | %s |" (map unBreakSpaces [p, ppTeamOrMultiple tom])) v,
    printf "| TOTALS | %s |" [
      (intercalate "<br>" . map (\(t,i) -> unBreakSpaces $ printf "- %s: %s" [t, show i]) . totalsPerSquad) v
    ]
  ]

positionGroupAndPrintTables :: Variation -> String
positionGroupAndPrintTables (Variation v) = intercalate "\n\n" . map (markDownTablePrintVariation . Variation) . groupBy (\a b -> getThird a == getThird b) $ v

-- | Generate MarkDown for a set of ProspectiveChanges and Variations
genMarkDown :: [(ProspectiveChange, Lineup, Variation)] -> String
genMarkDown plvs =
  let tableHead = newLineMap (\(pc,_,_) -> "<th>" ++ unBreakSpaces (ppProspectiveChange pc) ++ "</th>") plvs
      tableBody = concatMap (\(_,_,v) -> "<td style=\"vertical-align:top\">\n\n" ++ positionGroupAndPrintTables v ++ "\n\n</td>") plvs
   in intercalate "\n" [
    "<table>",
    "<tr>",
    tableHead,
    "</tr>",
    "<tr>",
    tableBody,
    "</tr>",
    "</table>"
   ]
