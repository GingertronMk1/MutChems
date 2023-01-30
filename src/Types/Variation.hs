module Types.Variation where

import Data.List
import Functions.Application
import Types.Basic
import Types.Player
import Types.TeamOrMultiple

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

variationToList :: Variation -> [VariationPlayer]
variationToList (Variation v) = v

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

flatLineupToVariations :: [Player] -> [Variation]
flatLineupToVariations =
  map Variation
    . mapM playerToVariationPlayers
