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

-- | An object containing a Lineup, the ProspectiveChange that has led to it,
-- and the best Variation - according to the functions defined below - of that
-- Lineup
data DisplayObject = DisplayObject
  { displayObjectLineup :: Lineup,
    displayObjectProspectiveChange :: ProspectiveChange,
    displayObjectVariation :: Variation
  }
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
toNumerical :: [(Team, Int)] -> (Int, Int)
toNumerical cv
  | bestT /= legends && bestN >= 50 = (4, bestN)
  | bestT /= legends && bestN >= 40 = (3, bestN)
  | bestT == legends && bestN >= 40 = (2, bestN)
  | otherwise = (1, bestN)
  where
    (bestT, bestN) = maximumBy (comparing snd) cv

-- | Taking a Variation and reducing it to just the list of Teams it contains
variationToTeams :: Variation -> [Team]
variationToTeams (Variation v) = sort . concatMap (expandTeamOrMultiple . vpTeam) $ v

-- | Take a Lineup and convert it to a list of all Variations
lineupToVariations :: Lineup -> Int -> [Variation]
lineupToVariations n =
  map Variation
    . mapM playerToVariationPlayers
    . convertSquad n

-- | Taking an individual Player and splitting out all of their Teams into a list
-- of VariationPlayers
playerToVariationPlayers :: Player -> [VariationPlayer]
playerToVariationPlayers p = [VP {vpName = pName p, vpTeam = t, vpPosition = pPosition p} | t <- pTeams p]

-- | Convert a Lineup to its best Variation according to the `compare` function
-- defined above
lineupToBestVariation :: Lineup -> Int -> Variation
lineupToBestVariation n = maximum . lineupToVariations n

-- | Generate the best Variations for a set of Lineups and add to the tuples
bestOfAllSquadsFn :: Int -> [BuildObject] -> [DisplayObject]
bestOfAllSquadsFn n = map (bestOfOneSquadFn n)

-- | Generate the best Variation for a given Lineup and add it to the provided Tuple
bestOfOneSquadFn :: Int -> BuildObject -> DisplayObject
bestOfOneSquadFn n (BuildObject {buildObjectLineup = l, buildObjectProspectiveChange = pc}) =
  DisplayObject
    { displayObjectLineup = l,
      displayObjectProspectiveChange = pc,
      displayObjectVariation = recursiveGetBestSquads l n
    }

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
recursiveGetBestSquads :: Lineup -> Int -> Variation
recursiveGetBestSquads l n =
  let ret@(Variation bestSquad) = lineupToBestVariation l n
      (noTeams, hasTeams) = partition ((NoTeam ==) . vpTeam) bestSquad
   in if null noTeams
        then ret
        else
          let hasTeamsLineup = map variationPlayerToLineupPlayer hasTeams
              noTeamsLineup = filter ((`elem` map vpName noTeams) . pName) l
              newLineup = sortBy (compareBasedOnSquad l) (hasTeamsLineup ++ noTeamsLineup)
           in recursiveGetBestSquads newLineup n
