-- | Module: Types.InitObject
module Types.InitObject where

import Classes.Data
import Data.List
import Functions.Application
import Types.ArgumentList
import Types.BuildObject
import Types.Lineup
import Types.Player
import Types.ProspectiveChange
import System.IO

-- | The InitObject, what we get out of the JSON file
data JSONInitObject = JSONInitObject
  { groupedLineup :: GroupedLineup,
    prospectiveChanges :: [ProspectiveChange]
  }
  deriving (Show)

instance Data JSONInitObject where
  toData (JSONInitObject {groupedLineup = gl, prospectiveChanges = pcs}) =
    dropFromEndWhile (== '\n')
      . intercalate "\n"
      $ [ toData gl,
          "===",
          intercalate "\n\n" . map toData $ pcs
        ]
  fromData s =
    let [gl, pcs] = take 2 . splitOnInfix "\n===\n" . dropFromEndWhile (== '\n') $ s
     in JSONInitObject
          { groupedLineup = fromData gl,
            prospectiveChanges =
              map fromData
                . filter (not . null . lines)
                . splitOnInfix "\n\n"
                $ pcs
          }
openAndStepInitObject :: String -> Int -> IO JSONInitObject
openAndStepInitObject s n = do
  fileContents <- readFile' s
  let firstInitObject = fromData fileContents
  let resultantInitObject = stepInitObject n firstInitObject
  writeFile s (toData resultantInitObject)
  return resultantInitObject

-- | Step an InitObject, applying the first ProspectiveChange to the lineup
stepInitObject :: Int -> JSONInitObject -> JSONInitObject
stepInitObject
  n
  ( JSONInitObject
      { groupedLineup = gl,
        prospectiveChanges = pcs
      }
    ) =
    let initialFlatLineup = flattenGroupedLineup gl
        (changesToApply, remainingChanges) = splitAt n pcs
     in JSONInitObject
          { groupedLineup =
              groupFlatLineup
                . sortOn (playerPositionInInitialLineup initialFlatLineup . playerName)
                . buildObjectLineup
                . last
                . iterativelyApplyProspectiveChanges changesToApply
                $ initialFlatLineup,
            prospectiveChanges = remainingChanges
          }

initObjectToBuildObjects :: JSONInitObject -> [BuildObject]
initObjectToBuildObjects (JSONInitObject {groupedLineup = gl, prospectiveChanges = pcs}) =
  iterativelyApplyProspectiveChanges pcs . flattenGroupedLineup $ gl
