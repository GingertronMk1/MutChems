module Types.Variation where

-- Module: Types.Variation
import Data.List
import Functions.Application
import Types.Basic
import Types.Player
import Types.TeamOrMultiple

-- | The Variation, a list of Players with one TeamOrMultiple each
newtype Variation = Variation [VariationPlayer]
  deriving (Eq, Show)

instance Ord Variation where
  compare v1 v2
    | ord1 /= ord2 = compare ord1 ord2
    | n1 /= n2 = compare n1 n2
    | otherwise =
      fst $
        orderListOfInts
          (map snd converted1)
          (map snd converted2)
    where
      converted1 = teamsInVariation v1
      converted2 = teamsInVariation v2
      (ord1, n1) = toNumerical converted1
      (ord2, n2) = toNumerical converted2

-- | Get a list of all represented teams and how many there are in a given Variation
teamsInVariation :: Variation -> [(Team, Int)]
teamsInVariation =
  firstAndLength
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

printVariationAsHtmlTable :: Variation -> String
printVariationAsHtmlTable (Variation v) =
  intercalate "\n" $ printVariationAsHtmlTable' "" v

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
        posRow = wrapInTag "tr" . wrapInTag "th colspan=2" $ pPos
     in if pPos == pos
          then thisRow : nextRow
          else posRow : thisRow : nextRow
