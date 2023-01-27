-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC8
import Data.Char
import Data.List
import Data.Other
import Data.Positions
import Data.Squad
import Types.Basic
import Types.JSON.Lineup
import Types.ProspectiveChange
import Types.TeamOrMultiple

-- | The squad with the team strategy item sorted
processedStrategy :: [Player]
processedStrategy = case strategy of
  NoTeam -> []
  tom ->
    [ emptyPlayer
        { pName = "STRATEGY: " ++ ppTeamOrMultiple tom,
          pTeams = [tom],
          pPosition = strategyCard
        }
    ]

-- | The maximum number of Variations per Lineup
squadFilterThreshold :: Int
squadFilterThreshold =
  read
    . filter isDigit
    $ squadFilterThresholdString

fromJSONInit :: IO JSONInitObject
fromJSONInit = do
  possibleInput <- fromJSONInit'
  case possibleInput of
    Left s -> error s
    Right jsio -> return jsio

fromJSONInit' :: IO (Either String JSONInitObject)
fromJSONInit' = do
  input <- readFile "input.json"
  return $ eitherDecode . BSC8.pack $ input

iteratedProspectiveSquads :: IO [BuildObject]
iteratedProspectiveSquads = do
  possibleInput <- fromJSONInit'
  case possibleInput of
    Left s -> error s
    Right jsio ->
      return
        . initObjectToBuildObjects
        . sortLineupInJSONInitObject
        $ jsio

sortMyInput :: IO ()
sortMyInput = do
  myIn <- fromJSONInit' 
  case myIn of
    Left s -> error s
    Right jsio -> sortMyInput' jsio

sortMyInput' :: JSONInitObject -> IO ()
sortMyInput' jsio = do
  let sortedJSIO = sortLineupInJSONInitObject jsio
  writeFile "sideput.json" . BSC8.unpack . encode $ sortedJSIO

