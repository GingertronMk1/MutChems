-- | Module: Types.DisplayObject
module Types.DisplayObject where

import Data.List
import Data.Ord
import Functions.Application
import Text.Printf
import Types.Basic
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
      { displayObjectVariation = (Variation var)
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
      $ var

-- | Print a list of DisplayObjects to one large HTML table
printDisplayObjectsAsHtmlTable :: [DisplayObject] -> String
printDisplayObjectsAsHtmlTable =
  wrapInTag "table" . concat
    . reverseMap
      [ printDisplayObjectsAsTableHead,
        printDisplayObjectsAsTableBody,
        printDisplayObjectsAsTableFoot
      ]

printDisplayObjectsAsTableBody :: [DisplayObject] -> String
printDisplayObjectsAsTableBody =
  wrapInTag "tbody"
    . newLineMap
      ( wrapInTag "td"
          . printDisplayObjectAsHtmlTable
      )

printDisplayObjectsAsTableHead :: [DisplayObject] -> String
printDisplayObjectsAsTableHead =
  wrapInTag "thead"
    . wrapInTag "tr"
    . newLineMap
      ( wrapInTag "th"
          . ppProspectiveChange
          . displayObjectProspectiveChange
      )

printDisplayObjectsAsTableFoot :: [DisplayObject] -> String
printDisplayObjectsAsTableFoot =
  wrapInTag "tfoot"
    . wrapInTag "tr"
    . newLineMap
      ( wrapInTag "td"
          . wrapInTag "ul"
          . concatMap
            ( wrapInTag "li"
                . unBreakCharacters
                . uncurry (printf "%s: %d")
            )
          . getTeamCountsFromDisplayObject
      )

getTeamCountsFromDisplayObject :: DisplayObject -> [(Team, Int)]
getTeamCountsFromDisplayObject (DisplayObject {displayObjectVariation = (Variation var)}) =
  sortOn
    (Down . snd)
    [ (head ts, length ts)
      | ts <-
          group
            . sort
            . concatMap (teamOrMultipleToTeams . variationPlayerTeam)
            $ var
    ]
