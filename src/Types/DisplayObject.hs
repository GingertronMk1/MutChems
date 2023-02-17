-- | Module: Types.DisplayObject
module Types.DisplayObject where

import Data.List
import Data.Ord
import Functions.Application
import Types.Printable
import Types.BuildObject
import Types.Lineup
import Types.Player
import Types.ProspectiveChange
import Types.Team
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

-- | Converting a build object to an intermediate object, being passed in a threshold
-- for the filtering
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
      . printVariationAsHtmlTable
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

-- | Printing the table body of a display object
printDisplayObjectsAsTableBody :: [DisplayObject] -> String
printDisplayObjectsAsTableBody =
  wrapInTag "tbody"
    . newLineMap
      ( wrapInTag "td"
          . printDisplayObjectAsHtmlTable
      )

-- | Printing the prospective change involved in a display object as a table head
printDisplayObjectsAsTableHead :: [DisplayObject] -> String
printDisplayObjectsAsTableHead =
  wrapInTag "thead"
    . wrapInTag "tr"
    . newLineMap
      ( wrapInTag "th"
          . ppProspectiveChange
          . displayObjectProspectiveChange
      )

-- | Printing the list of team chemistries as a table foot
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
                . (\(t, n) -> printf "%s: %s" (show t) n)
            )
          . getTeamCountsFromDisplayObject
      )

-- | Getting the number of each Team represented in a DisplayObject
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
