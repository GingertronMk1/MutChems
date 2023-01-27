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

-- | Just the base squad and strategy item
squadNoProspectives :: Lineup
squadNoProspectives =
  filter (not . null . pTeams)
    . (++ processedStrategy)
    . streamlineLineup
    $ baseSquad

-- | The generated list of squads in "chronological" order (or at least planned)
iteratedProspectiveSquads :: [BuildObject]
iteratedProspectiveSquads =
  addProspectivesInTurn
    prospectiveAdditions
    squadNoProspectives

-- | The maximum number of Variations per Lineup
squadFilterThreshold :: Int
squadFilterThreshold =
  read
    . filter isDigit
    $ squadFilterThresholdString

squadsMinusTeam :: Team -> [BuildObject]
squadsMinusTeam t =
  map (\bo@(BuildObject {buildObjectLineup = bol}) -> bo {buildObjectLineup = filterOutTeam t bol}) iteratedProspectiveSquads

toJSONLineup :: IO ()
toJSONLineup = do
  let jsonLineups = lineupToJSONLineup . buildObjectLineup . head $ iteratedProspectiveSquads
  writeFile "output.json" . BSC8.unpack . encode $ jsonLineups

toJSONInit :: IO ()
toJSONInit = do
  let jsonSquad = lineupToJSONLineup squadNoProspectives
  let jsonProspectiveChanges = map prospectiveChangeToJSONProspectiveChange prospectiveAdditions
  let jsonInitObject = JSONInitObject {
    jsonIOSquad = jsonSquad,
    jsonIOProspectiveChanges = jsonProspectiveChanges
  }
  writeFile "output.json" . BSC8.unpack . encode $ jsonInitObject

fromJSONInit :: IO ()
fromJSONInit = do
  input <- readFile "input.json"
  let possibleInput = eitherDecode . BSC8.pack $ input :: Either String JSONInitObject
  case possibleInput of
    Left s -> putStrLn s
    Right jsio -> print jsio
