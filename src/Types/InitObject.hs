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
          "===",
          intercalate "\n\n" . map toData $ pcs
        ]
  fromData s =
    let [gl, pcs] = take 2
                  . splitOnInfix "\n===\n"
                  . dropFromEndWhile (== '\n')
                  . unlines
                  . removeWhitespaceLines
                  . lines
                  $ s
     in InitObject
          { groupedLineup = fromData gl,
            prospectiveChanges =
              map fromData
                . filter (not . null . lines)
                . splitOnInfix "\n\n"
                $ pcs
          }

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

initObjectToBuildObjects :: InitObject -> [BuildObject]
initObjectToBuildObjects (InitObject {groupedLineup = gl, prospectiveChanges = pcs}) =
  iterativelyApplyProspectiveChanges pcs . flattenGroupedLineup $ gl
