module Types.DisplayObject where

import Functions.Application
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
      { displayObjectVariation = var
      }
    ) =
    wrapInTag "table"
      . newLineMap
        ( \(VariationPlayer {variationPlayerName = vpn, variationPlayerTeam = vpt}) ->
            wrapInTag "tr"
              . concatMap (wrapInTag "td" . unBreakCharacters)
              $ [ vpn,
                  ppTeamOrMultiple vpt
                ]
        )
      . variationToList
      $ var

printDisplayObjectsAsHtmlTable :: [DisplayObject] -> String
printDisplayObjectsAsHtmlTable dos =
  wrapInTag "table" $
    ( wrapInTag "thead"
        . wrapInTag "tr"
        . newLineMap (wrapInTag "th" . ppProspectiveChange . displayObjectProspectiveChange)
        $ dos
    )
      ++ ( wrapInTag "tbody"
             . newLineMap
               (wrapInTag "td" . printDisplayObjectAsHtmlTable)
             $ dos
         )
