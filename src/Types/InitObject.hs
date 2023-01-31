{-# LANGUAGE DeriveGeneric #-}

module Types.InitObject where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List
import GHC.Generics
import Types.ArgumentList
import Types.BuildObject
import Types.Lineup
import Types.Player
import Types.ProspectiveChange

data JSONInitObject = JSONInitObject
  { groupedLineup :: GroupedLineup,
    prospectiveChanges :: [ProspectiveChange]
  }
  deriving (Show, Generic)

instance FromJSON JSONInitObject

instance ToJSON JSONInitObject

decodeJSONInitObject :: String -> IO JSONInitObject
decodeJSONInitObject s = do
  teamJSON <- BS.readFile s
  case eitherDecode teamJSON of
    Left err -> error err
    Right tj -> return tj

stepInitObject :: ArgumentList -> JSONInitObject -> IO JSONInitObject
stepInitObject (ArgumentList {argInputFile = inputFile, argStepCount = stepCount}) jsio =
  do
    if stepCount > 0
      then do
        let steppedInitObject = stepInitObject' stepCount jsio
        BS.writeFile inputFile . encode $ steppedInitObject
        return steppedInitObject
      else return jsio

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
