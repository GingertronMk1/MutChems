{-# LANGUAGE DeriveGeneric #-}
module Types.InitObject where

import Data.Aeson
import Types.Lineup
import qualified Data.ByteString.Lazy as BS
import Types.ProspectiveChange
import GHC.Generics

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

