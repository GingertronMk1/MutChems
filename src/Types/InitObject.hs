{-# LANGUAGE DeriveGeneric #-}

module Types.InitObject where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import Types.Lineup
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
