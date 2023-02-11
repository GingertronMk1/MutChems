-- | Module: Types.InitObject
module Types.InitObject where

import Classes.Data
import Data.List
import Functions.Application
import System.IO
import Types.BuildObject
import Types.Lineup
import Types.Player
import Types.ProspectiveChange

-- | The InitObject, what we get out of the JSON file
data InitObject = InitObject
  { groupedLineup :: GroupedLineup,
    prospectiveChanges :: [ProspectiveChange]
  }
  deriving (Show)

instance Data InitObject where
  toData (InitObject {groupedLineup = gl, prospectiveChanges = pcs}) =
    dropFromEndWhile (== '\n')
      . intercalate "\n"
      $ [ toData gl,
          "\n===\n",
          intercalate "\n\n" . map toData $ pcs
        ]
  fromData s =
    case take 2
      . splitOnInfix "==="
      . dropFromEndWhile (== '\n')
      . unlines
      . removeWhitespaceLines
      . lines
      $ s of
      [gl, pcs] ->
        InitObject
          { groupedLineup = fromData gl,
            prospectiveChanges =
              map fromData
                . filter (not . null . lines)
                . splitOnInfix "\n\n"
                $ pcs
          }
      [gl] -> InitObject {groupedLineup = fromData gl, prospectiveChanges = []}
      _ -> error s

-- | A function to first open a data file and then step and return an InitObject,
-- also writing the stepped object back to the file
openAndStepInitObject :: String -> Int -> IO InitObject
openAndStepInitObject s n = do
  fileContents <- readFile' s
  let firstInitObject = fromData fileContents
  let resultantInitObject = stepInitObject n firstInitObject
  writeFile s (toData resultantInitObject)
  return resultantInitObject

-- | Step an InitObject, applying the first ProspectiveChange to the lineup
stepInitObject :: Int -> InitObject -> InitObject
stepInitObject
  n
  ( InitObject
      { groupedLineup = gl,
        prospectiveChanges = pcs
      }
    ) =
    let initialFlatLineup = flattenGroupedLineup gl
        (changesToApply, remainingChanges) = splitAt n pcs
     in InitObject
          { groupedLineup =
              groupFlatLineup
                . sortOn (playerPositionInInitialLineup initialFlatLineup . playerName)
                . buildObjectLineup
                . last
                . iterativelyApplyProspectiveChanges changesToApply
                $ initialFlatLineup,
            prospectiveChanges = remainingChanges
          }

-- | Covert an initObject to a list of buildObjects
initObjectToBuildObjects :: InitObject -> [BuildObject]
initObjectToBuildObjects (InitObject {groupedLineup = gl, prospectiveChanges = pcs}) =
  iterativelyApplyProspectiveChanges pcs . flattenGroupedLineup $ gl
