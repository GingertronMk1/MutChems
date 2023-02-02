-- | Module: Types.DisplayObject
module Types.DisplayObject where

import Functions.Application
import Types.BuildObject
import Types.Lineup
import Types.Player
import Types.ProspectiveChange
import Types.TeamOrMultiple
import Types.Variation

-- | An intermediate Object, containing a list of Variations and the ProspectiveChange
data IntermediateObject = IntermediateObject
  { iObjVariations :: [Variation],
    iObjProspectiveChange :: ProspectiveChange
  }
  deriving (Show)

-- | The DisplayObject, something to print to HTML
data DisplayObject = DisplayObject
  { displayObjectVariation :: Variation,
    displayObjectProspectiveChange :: ProspectiveChange
  }
  deriving (Show)

buildObjectToIntermediateObject :: Int -> BuildObject -> IntermediateObject
buildObjectToIntermediateObject
  n
  ( BuildObject
      { buildObjectLineup = l,
        buildObjectProspectiveChange = pc
      }
    ) =
    IntermediateObject
      { iObjVariations = flatLineupToVariations . reduceFlatLineupRecursive n $ l,
        iObjProspectiveChange = pc
      }

-- | Converting a BuildObject to its best possible DisplayObject
intermediateObjectToDisplayObject :: IntermediateObject -> DisplayObject
intermediateObjectToDisplayObject
  ( IntermediateObject
      { iObjVariations = vars,
        iObjProspectiveChange = pc
      }
    ) = DisplayObject {displayObjectVariation = maximum vars, displayObjectProspectiveChange = pc}

-- | Print a given DisplayObject as an HTML Table
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

-- | Print a list of DisplayObjects to one large HTML table
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
