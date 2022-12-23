-- | Module: Types.Variation
module Types.Variation where

import Data.List
import Data.Ord
import Data.Teams
import Functions.Application
import Types.Basic
import Types.ProspectiveChange
import Types.TeamOrMultiple

-- | The object representing a Variation of a squad
newtype Variation
  = Variation [VariationPlayer]
  deriving (Eq, Show)

data AnalysisObject = A
  { aLineup :: Lineup,
    aProspectiveChange :: ProspectiveChange,
    aVariation :: Variation
  }
  deriving (Eq, Show)

instance Ord Variation where
  compare v1 v2 =
    let converted1 = teamsInVariation v1
        converted2 = teamsInVariation v2
     in case compare (toNumerical converted1) (toNumerical converted2) of
          EQ -> fst $ orderListOfInts (map snd converted1) (map snd converted2)
          nonEQ -> nonEQ

-- | The players involved in a Variation - identical to the t`Types.TeamOrMultiple.Player`
-- except only one Team allowed
data VariationPlayer = VP
  { vpName :: PlayerName,
    vpTeam :: TeamOrMultiple,
    vpPosition :: Position
  }
  deriving (Eq, Show)

-- | Expanding a full lineup to get all options
expandLineup :: Lineup -> [[VariationPlayer]]
expandLineup = map playerToVariationPlayers

-- | Get a list of all represented teams and how many there are in a given Variation
teamsInVariation :: Variation -> [(Team, Int)]
teamsInVariation =
  firstAndLength
    . variationToTeams

-- | Take a Team and how many there are and convert it into an integer so we can
-- more easily compare it to others - ordering them in priority
toNumerical :: [(Team, Int)] -> Int
toNumerical cv
  | bestT /= legends && bestN >= 50 = 4
  | bestT /= legends && bestN >= 40 = 3
  | bestT == legends && bestN >= 40 = 2
  | otherwise = 1
  where
    (bestT, bestN) = maximumBy (comparing snd) cv

-- | Taking a Variation and reducing it to just the list of Teams it contains
variationToTeams :: Variation -> [Team]
variationToTeams (Variation v) = sort . concatMap (expandTeamOrMultiple . vpTeam) $ v

-- | Take a Lineup and convert it to a list of all Variations
lineupToVariations :: Lineup -> [Variation]
lineupToVariations =
  map Variation
    . mapM playerToVariationPlayers
    . convertSquad

playerToVariationPlayers :: Player -> [VariationPlayer]
playerToVariationPlayers p = [VP {vpName = pName p, vpTeam = t, vpPosition = pPosition p} | t <- pTeams p]

-- | Convert a Lineup to its best Variation according to the `compare` function
-- defined above
lineupToBestVariation :: Lineup -> Variation
lineupToBestVariation = maximum . lineupToVariations

-- | Generate the best Variations for a set of Lineups and add to the tuples
bestOfAllSquadsFn :: [LineupAndProspectiveChange] -> [AnalysisObject]
bestOfAllSquadsFn = map bestOfOneSquadFn

-- | Generate the best Variation for a given Lineup and add it to the provided Tuple
bestOfOneSquadFn :: LineupAndProspectiveChange -> AnalysisObject
bestOfOneSquadFn (LAPC {lapcLineup = l, lapcProspectiveChange = pc}) =
  A {aLineup = l, aProspectiveChange = pc, aVariation = recursiveGetBestSquads l}

-- | Using the totals of each team in each Variation, kind of unfolding them?.
totalsPerSquad :: [VariationPlayer] -> [(Team, Int)]
totalsPerSquad =
  sortOn (Down . snd)
    . firstAndLength
    . concatMap (expandTeamOrMultiple . vpTeam)

-- | List up the single Team belonging to a Variation player
variationPlayerToLineupPlayer :: VariationPlayer -> Player
variationPlayerToLineupPlayer vp =
  P
    { pName = vpName vp,
      pTeams = [vpTeam vp],
      pPosition = vpPosition vp
    }

-- | Generate a fully populated variation
recursiveGetBestSquads :: Lineup -> Variation
recursiveGetBestSquads l =
  let ret@(Variation bestSquad) = lineupToBestVariation l
      (noTeams, hasTeams) = partition ((NoTeam ==) . vpTeam) bestSquad
   in if null noTeams
        then ret
        else
          let hasTeamsLineup = map variationPlayerToLineupPlayer hasTeams
              noTeamsLineup = filter ((`elem` map vpName noTeams) . pName) l
              newLineup = sortBy (compareBasedOnSquad l) (hasTeamsLineup ++ noTeamsLineup)
           in recursiveGetBestSquads newLineup
