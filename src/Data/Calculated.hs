-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import qualified Data.ByteString.Lazy.Char8 as BSC8
import Data.Aeson
import Data.Char
import Data.Other
import Data.Positions
import Data.List
import Data.Squad
import Types.Basic
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

jsonedSquads :: IO ()
jsonedSquads =
  do
    let squads = BSC8.unpack
               . encode
               . buildObjectLineup
               . head
               $ iteratedProspectiveSquads
    writeFile "output.json" squads

fromJSONSquads :: IO()
fromJSONSquads = do
  jsonSquads <- BSC8.readFile "output.json"
  -- print jsonSquads
  let squads = eitherDecode jsonSquads :: Either String Lineup
  case squads of
    Left s -> putStrLn s
    Right s -> print s