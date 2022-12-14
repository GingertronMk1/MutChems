-- | Module: Functions.Display.
module Functions.Display where

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
  surroundInTag "table"
  . concat
  $ [
    -- The head of the table, just 2 cells saying "Player" and "Chemistry"
    surroundInTag "thead"
    . surroundInTag "tr"
    . concatMap (surroundInTag "th")
    $ ["Player", "Chemistry"],
    -- The body of the table, containing all players and their chemistry in this Variation
    surroundInTag "tbody" $ htmlTablePrintVariation' v,
    -- The foot of the table, containing a list of all chemistries and how many there are
    surroundInTag "tfoot"
    . surroundInTag "tr"
    . concatMap (surroundInTag "td")
    $ [
      "TOTALS",
      surroundInTag "ul"
      . newLineMap (\(t,i) -> unBreakSpaces . surroundInTag "li" $ t ++ ": " ++ show i)
      . totalsPerSquad
      $ v
    ]
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
      else (surroundInTag "tr" . surroundInTag "td colspan=2" . surroundInTag "b" $ position) : thisLine : htmlTablePrintVariation'' position ps


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
surroundInTag openingTag content =
  let (tag:attributes) = words openingTag
   in concat [
        "<",
        tag,
        concatMap (' ':) attributes,
        ">",
        content,
        "</",
        tag,
        ">"
      ]

ppNumberOfPlayersOnTeam :: Lineup -> Team -> String
ppNumberOfPlayersOnTeam l t = let (ins, outs) = numberOfPlayersOnTeam l t
                                  ppPlayer (p,_,pos) = printf "| %s | %s |" [p,pos]
                                  ppPlayers ps = intercalate "\n" [
                                      "| Player | Position |",
                                      "|:---|---:|",
                                      newLineMap ppPlayer ps
                                    ]
                               in intercalate "\n" [
                                printf "Has %s chemistry" [t],
                                "",
                                ppPlayers ins,
                                "",
                                printf "Does not have %s chemistry" [t],
                                "",
                                ppPlayers outs
                               ]

ppNumberOfPlayersOnEveryTeam :: Lineup -> String
ppNumberOfPlayersOnEveryTeam l =
  let allTeams = sort . nub . allTeamsFn $ l
   in intercalate "\n\n---\n\n"
    . map (\t -> printf "# %s\n\n%s" [t, ppNumberOfPlayersOnTeam l t])
    $ allTeams