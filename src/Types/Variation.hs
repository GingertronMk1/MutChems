-- | Module: Types.Variation
module Types.Variation where

import Data.List
import Data.Ord
import Functions.Application
import Types.Player
import Types.Position
import Types.Team
import Types.TeamOrMultiple

-- | The Variation, a list of Players with one TeamOrMultiple each
newtype Variation = Variation [VariationPlayer]
  deriving (Eq, Show)

instance Ord Variation where
  compare v1 v2
    | and
        [ team1 == Legends,
          team2 == Legends,
          teamN1 >= 40,
          teamN2 >= 40
        ]
        || and
          [ team1 /= Legends,
            team2 /= Legends,
            teamN1 >= 50,
            teamN2 >= 50
          ]
        || and
          [ team1 /= Legends,
            team2 /= Legends,
            teamN1 >= 40,
            teamN2 >= 40
          ] =
      compare nextV1 nextV2
    | ord1 /= ord2 =
      compare ord1 ord2
    | n1 /= n2 =
      compare n1 n2
    | playersPerTeam1 /= playersPerTeam2 =
      compare
        playersPerTeam2
        playersPerTeam1
    | otherwise =
      fst $
        orderListOfInts
          (map snd converted1)
          (map snd converted2)
    where
      nextV1 = removeTeamFromVariation team1 v1
      nextV2 = removeTeamFromVariation team2 v2
      converted1 = teamsInVariation v1
      converted2 = teamsInVariation v2
      (team1, teamN1) = head converted1
      (team2, teamN2) = head converted2
      (ord1, n1) = toNumerical converted1
      (ord2, n2) = toNumerical converted2
      playersPerTeam1 = meanPlayersPerTeam v1
      playersPerTeam2 = meanPlayersPerTeam v2

-- | Removing all players solely belonging to a given team from a variation
removeTeamFromVariation :: Team -> Variation -> Variation
removeTeamFromVariation t (Variation v) =
  Variation $ filter (not . variationPlayerBelongsToTeam t) v

-- | Does a variation player belong to this team and this team only?
variationPlayerBelongsToTeam :: Team -> VariationPlayer -> Bool
variationPlayerBelongsToTeam t vp =
  (nub . expandTeamOrMultiple . variationPlayerTeam $ vp) == [t]

-- | Get the average number of players per Team in a Variation
meanPlayersPerTeam :: Variation -> Float
meanPlayersPerTeam = mean . map snd . playersPerTeam

-- | The number of players belonging to each team represented in the variation
playersPerTeam :: Variation -> [(Team, Int)]
playersPerTeam v =
  let allTeams = variationToTeams v
   in map (playersInAGivenTeam v) allTeams

-- | The number of players belonging to a given team in a variation
playersInAGivenTeam :: Variation -> Team -> (Team, Int)
playersInAGivenTeam v t = (t, length (playersBelongingToTeam v t))

-- | The list of players belonging to a given team in a variation
playersBelongingToTeam :: Variation -> Team -> [VariationPlayer]
playersBelongingToTeam (Variation v) t =
  filter (\(VariationPlayer {variationPlayerTeam = vpt}) -> t `elem` expandTeamOrMultiple vpt) v

-- | Get a list of all represented teams and how many there are in a given Variation
teamsInVariation :: Variation -> [(Team, Int)]
teamsInVariation =
  sortOn (Down . snd)
    . firstAndLength
    . variationToTeams

-- | Taking a Variation and reducing it to just the list of Teams it contains
variationToTeams :: Variation -> [Team]
variationToTeams (Variation v) =
  sort
    . concatMap
      ( expandTeamOrMultiple
          . variationPlayerTeam
      )
    $ v

-- | Convert a FlatLineup into a list of its available Variations
flatLineupToVariations :: [Player] -> [Variation]
flatLineupToVariations =
  map Variation
    . mapM playerToVariationPlayers

-- | Print a variation as an HTML table
printVariationAsHtmlTable :: Variation -> String
printVariationAsHtmlTable (Variation v) =
  intercalate "\n" $ printVariationAsHtmlTable' NoPosition v

-- | Print each player in a variation as a row in an HTML table, with new rows
-- for each position change
printVariationAsHtmlTable' :: Position -> [VariationPlayer] -> [String]
printVariationAsHtmlTable' _ [] = []
printVariationAsHtmlTable'
  pos
  ( VariationPlayer
      { variationPlayerName = pName,
        variationPlayerTeam = pTeam,
        variationPlayerPosition = pPos
      }
      : ps
    ) =
    let thisRow =
          wrapInTag "tr"
            . concatMap (wrapInTag "td")
            $ [ unBreakCharacters pName,
                unBreakCharacters . ppTeamOrMultiple $ pTeam
              ]
        nextRow = printVariationAsHtmlTable' pPos ps
        posRow = wrapInTag "tr" . wrapInTag "th colspan=2" . show $ pPos
     in if pPos == pos
          then thisRow : nextRow
          else posRow : thisRow : nextRow
