module Types.Variation where

-- Module: Types.Variation
import Data.List
import Data.Ord
import Functions.Application
import Types.Basic
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
            ]
        =
      compare nextV1 nextV2
    | ord1 /= ord2 = compare ord1 ord2
    | n1 /= n2 = compare n1 n2
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

removeTeamFromVariation :: TeamData -> Variation -> Variation
removeTeamFromVariation td (Variation v) =
  Variation $ filter (\VariationPlayer {variationPlayerTeam = vpt} -> (nub . expandTeamOrMultiple $ vpt) /= [td]) v

-- | Get a list of all represented teams and how many there are in a given Variation
teamsInVariation :: Variation -> [(TeamData, Int)]
teamsInVariation =
  sortOn (Down . snd)
    . firstAndLength
    . variationToTeams

-- | Taking a Variation and reducing it to just the list of Teams it contains
variationToTeams :: Variation -> [TeamData]
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
