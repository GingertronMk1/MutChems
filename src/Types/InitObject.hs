{-# LANGUAGE DeriveGeneric #-}

-- | Module: Types.InitObject
module Types.InitObject where

import Classes.Data
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List
import Functions.Application
import GHC.Generics
import Types.ArgumentList
import Types.BuildObject
import Types.Lineup
import Types.Player
import Types.ProspectiveChange

-- | The InitObject, what we get out of the JSON file
data JSONInitObject = JSONInitObject
  { groupedLineup :: GroupedLineup,
    prospectiveChanges :: [ProspectiveChange]
  }
  deriving (Show, Generic)

instance FromJSON JSONInitObject

instance ToJSON JSONInitObject

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

-- | Decoding the InitObject from a JSON file
decodeJSONInitObject :: String -> IO JSONInitObject
decodeJSONInitObject s = do
  teamJSON <- BS.readFile s
  case eitherDecode teamJSON of
    Left err -> error err
    Right tj -> return tj

-- | If we step an InitObject, write it to a file and return it
stepInitObject :: ArgumentList -> JSONInitObject -> IO JSONInitObject
stepInitObject (ArgumentList {argInputFile = inputFile, argStepCount = stepCount}) jsio =
  do
    if stepCount > 0
      then do
        let steppedInitObject = stepInitObject' stepCount jsio
        writeToFile inputFile steppedInitObject
        return steppedInitObject
      else return jsio

-- | Step an InitObject, applying the first ProspectiveChange to the lineup
stepInitObject' :: Int -> JSONInitObject -> JSONInitObject
stepInitObject'
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
