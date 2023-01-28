-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC8
import Data.Char
import Data.Other
import Types.JSON.Lineup
import Types.ProspectiveChange

-- | The maximum number of Variations per Lineup
squadFilterThreshold :: Int
squadFilterThreshold =
  read
    . filter isDigit
    $ squadFilterThresholdString

-- | Read the JSON file containing the Team information and return it
fromJSONInit :: IO JSONInitObject
fromJSONInit = do
  input <- readFile "input.json"
  case eitherDecode . BSC8.pack $ input of
    Left s -> error s
    Right jsio -> return jsio

-- | Get the build objects from the JSON init objects
iteratedProspectiveSquads :: IO [BuildObject]
iteratedProspectiveSquads = do
  jsio <- fromJSONInit
  return
    . map sortLineupInBuildObject
    . initObjectToBuildObjects
    $ jsio

-- | Write to a file on the side a sorted version of the suqad
sortMyInput :: IO ()
sortMyInput = do
  myIn <- fromJSONInit
  writeFile "sideput.json"
    . BSC8.unpack
    . encode
    . sortLineupInJSONInitObject
    $ myIn
