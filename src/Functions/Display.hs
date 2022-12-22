-- | Module: Functions.Display.
module Functions.Display where

import Data.List
import Functions.Application
import Types.Basic
import Types.ProspectiveChange
import Types.TeamOrMultiple
import Types.Variation

-- | Print a Variation as a Html table
htmlTablePrintVariation :: Variation -> String
htmlTablePrintVariation (Variation v) =
  surroundInTag "table"
    . concat
    $ [
        -- The head of the table, just 2 cells saying "Player" and "Chemistry"
        surroundInTag "thead"
          . surroundInTag "tr"
          . removeNewLines
          . concatMap (surroundInTag "th")
          $ ["Player", "Chemistry"],
        -- The body of the table, containing all players and their chemistry in this Variation
        surroundInTag "tbody" $ htmlTablePrintVariation' v,
        -- The foot of the table, containing a list of all chemistries and how many there are
        surroundInTag "tfoot"
          . surroundInTag "tr"
          . concatMap (surroundInTag "td")
          $ [ "TOTALS",
              surroundInTag "ul"
                . newLineMap (\(t, i) -> removeNewLines . surroundInTag "li" . printf "%s: %s" . map unBreakCharacters $ [t, show i])
                . totalsPerSquad
                $ v
            ]
      ]

-- | Print the internals of a Variation
htmlTablePrintVariation' :: [VariationPlayer] -> String
htmlTablePrintVariation' = intercalate "\n" . htmlTablePrintVariation'' "none"

-- | Print each row of the table, breaking on position changes
htmlTablePrintVariation'' :: String -> [VariationPlayer] -> [String]
htmlTablePrintVariation'' _ [] = []
htmlTablePrintVariation'' oldPos ((VP {vpName = p, vpTeam = t, vpPosition = pos}) : ps) =
  let thisLine = removeNewLines . surroundInTag "tr" . concatMap (surroundInTag "td" . unBreakCharacters) $ [p, ppTeamOrMultiple t]
   in if pos == oldPos
        then thisLine : htmlTablePrintVariation'' oldPos ps
        else (removeNewLines . surroundInTag "tr" . surroundInTag "td colspan=2" . surroundInTag "b" $ pos) : thisLine : htmlTablePrintVariation'' pos ps

-- | Generate Html for a set of ProspectiveChanges and Variations
genHtml :: [(ProspectiveChange, Variation)] -> String
genHtml plvs =
  let tableHead = newLineMap (removeNewLines . surroundInTag "th" . unBreakCharacters . ppProspectiveChange . fst) plvs
      tableBody = concatMap (surroundInTag "td style=\"vertical-align:top\"" . htmlTablePrintVariation . snd) plvs
   in intercalate
        "\n"
        [ "<table>",
          surroundInTag "tr" tableHead,
          surroundInTag "tr" tableBody,
          "</table>"
        ]

-- | Pop a string in tags
surroundInTag :: String -> String -> String
surroundInTag openingTag content =
  let (tag : attributes) = words openingTag
   in intercalate
        "\n\n"
        [ printf "<%s%s>" [tag, concatMap (' ' :) attributes],
          content,
          printf "</%s>" [tag]
        ]

-- | Nicely print the number of Players with a given team chemistry in a Lineup
ppNumberOfPlayersOnTeam :: Lineup -> Team -> String
ppNumberOfPlayersOnTeam l t =
  let (ins, outs) = numberOfPlayersOnTeam l t
      ppPlayer (P {pName = p, pPosition = pos}) = printf "| %s | %s |" [p, pos]
      ppPlayers ps =
        intercalate
          "\n"
          [ "| Player | Position |",
            "|:---|---:|",
            newLineMap ppPlayer ps
          ]
      tag = printf "a id=\"%s\"" [t]
   in surroundInTag tag
        . intercalate "\n\n"
        $ [ printf "# %s - %s/%s" [t, show (length ins), show (length l)],
            printf "### Has %s chemistry" [t],
            ppPlayers ins,
            printf "### Does not have %s chemistry" [t],
            ppPlayers outs
          ]

-- | Nicely print the number of Players with each team chemistry in a Lineup
ppNumberOfPlayersOnEveryTeam :: Lineup -> String
ppNumberOfPlayersOnEveryTeam l =
  let allTeams = sort . nub . allTeamsFn $ l
   in intercalate "\n\n---\n\n"
        . map (ppNumberOfPlayersOnTeam l)
        $ allTeams

-- | Remove newline characters from a string
removeNewLines :: String -> String
removeNewLines = filter (/= '\n')
