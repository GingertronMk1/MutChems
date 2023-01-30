module Types.DisplayObject where

import Data.List
import Text.Printf
import Types.BuildObject
import Types.Lineup
import Types.Player
import Types.ProspectiveChange
import Types.TeamOrMultiple
import Types.Variation

data DisplayObject = DisplayObject
  { displayObjectVariation :: Variation,
    displayObjectProspectiveChange :: ProspectiveChange
  }
  deriving (Show)

ppDisplayObject :: DisplayObject -> String
ppDisplayObject (DisplayObject {displayObjectVariation = var}) =
  intercalate "\n"
    . map
      ( \(VariationPlayer {variationPlayerName = vpn, variationPlayerTeam = vpt, variationPlayerPosition = vpp}) ->
          printf "%s | %s | %s" vpn (show vpt) vpp
      )
    . variationToList
    $ var

buildObjectToDisplayObject :: Int -> BuildObject -> DisplayObject
buildObjectToDisplayObject n (BuildObject {buildObjectLineup = l, buildObjectProspectiveChange = pc}) =
  let newFlatLineup = reduceFlatLineupRecursive n l
   in DisplayObject
        { displayObjectVariation = maximum . flatLineupToVariations $ newFlatLineup,
          displayObjectProspectiveChange = pc
        }

printDisplayObjectAsHtmlTable :: DisplayObject -> String
printDisplayObjectAsHtmlTable
  ( DisplayObject
      { displayObjectVariation = var,
        displayObjectProspectiveChange = pc
      }
    ) =
    intercalate
      "\n"
      [ "<table>",
        printf "<tr><th colspan=2>%s</th></tr>" $ ppProspectiveChange pc,
        intercalate "\n"
          . map
            ( \(VariationPlayer {variationPlayerName = vpn, variationPlayerTeam = vpt}) ->
                printf "<tr><td>%s</td><td>%s</td></tr>" vpn (ppTeamOrMultiple vpt)
            )
          . variationToList
          $ var,
        "</table>"
      ]
