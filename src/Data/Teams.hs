-- | Module: Data.Teams
--
-- All the teams
module Data.Teams where

import Data.List
import Data.Maybe
import Data.List.Split
import Types.Basic
import Types.TeamOrMultiple

-- * Individual teams

-- | The Chicago Bears
bears :: Team
bears = "Bears"

-- | The Cincinnati Bengals
bengals :: Team
bengals = "Bengals"

-- | The Buffalo Bills
bills :: Team
bills = "Bills"

-- | The Denver Broncos
broncos :: Team
broncos = "Broncos"

-- | The Cleveland Browns
browns :: Team
browns = "Browns"

-- | The Tampa Bay Buccaneers
buccaneers :: Team
buccaneers = "Buccaneers"

-- | The Arizona Cardinals
cardinals :: Team
cardinals = "Cardinals"

-- | The LA Chargers
chargers :: Team
chargers = "Chargers"

-- | The Kansas City Chiefs
chiefs :: Team
chiefs = "Chiefs"

-- | The Indianapolis Colts
colts :: Team
colts = "Colts"

-- | The Washington Commanders
commanders :: Team
commanders = "Commanders"

-- | The Dallas Cowboys
cowboys :: Team
cowboys = "Cowboys"

-- | The Miami Dolphins
dolphins :: Team
dolphins = "Dolphins"

-- | The Philadelphia Eagles
eagles :: Team
eagles = "Eagles"

-- | The Atlanta Falcons
falcons :: Team
falcons = "Falcons"

-- | The New York Giants
giants :: Team
giants = "Giants"

-- | The Jacksonville Jaguars
jaguars :: Team
jaguars = "Jaguars"

-- | The New York Jets
jets :: Team
jets = "Jets"

-- | Legends
legends :: Team
legends = "Legends"

-- | The Detroit Lions
lions :: Team
lions = "Lions"

-- | The San Francisco 49ers
niners :: Team
niners = "49ers"

-- | The Green Bay Packers
packers :: Team
packers = "Packers"

-- | The Carolina Panthers
panthers :: Team
panthers = "Panthers"

-- | The New England Patriots
patriots :: Team
patriots = "Patriots"

-- | The Las Vegas Raiders
raiders :: Team
raiders = "Raiders"

-- | The LA Rams
rams :: Team
rams = "Rams"

-- | The Baltimore Ravens
ravens :: Team
ravens = "Ravens"

-- | The New Orleans Saints
saints :: Team
saints = "Saints"

-- | The Seattle Seahawks
seahawks :: Team
seahawks = "Seahawks"

-- | The Pittsburgh Steelers
steelers :: Team
steelers = "Steelers"

-- | The Houston Texans
texans :: Team
texans = "Texans"

-- | The Tennessee Titans
titans :: Team
titans = "Titans"

-- | The Minnesota Vikings
vikings :: Team
vikings = "Vikings"

-- * Divisions

-- | All of the teams in the NFC North
nfcNorth :: [TeamOrMultiple]
nfcNorth =
  map
    Team
    [ bears,
      lions,
      packers,
      vikings
    ]

-- | All of the teams in the NFC East
nfcEast :: [TeamOrMultiple]
nfcEast =
  map
    Team
    [ commanders,
      cowboys,
      eagles,
      giants
    ]

-- | All of the teams in the NFC South
nfcSouth :: [TeamOrMultiple]
nfcSouth =
  map
    Team
    [ buccaneers,
      falcons,
      panthers,
      saints
    ]

-- | All of the teams in the NFC West
nfcWest :: [TeamOrMultiple]
nfcWest =
  map
    Team
    [ cardinals,
      niners,
      rams,
      seahawks
    ]

-- | All of the teams in the AFC North
afcNorth :: [TeamOrMultiple]
afcNorth =
  map
    Team
    [ bengals,
      browns,
      ravens,
      steelers
    ]

-- | All of the teams in the AFC East
afcEast :: [TeamOrMultiple]
afcEast =
  map
    Team
    [ bills,
      dolphins,
      jets,
      patriots
    ]

-- | All of the teams in the AFC South
afcSouth :: [TeamOrMultiple]
afcSouth =
  map
    Team
    [ colts,
      jaguars,
      texans,
      titans
    ]

-- | All of the teams in the AFC West
afcWest :: [TeamOrMultiple]
afcWest =
  map
    Team
    [ broncos,
      chargers,
      chiefs,
      raiders
    ]

-- * Conferences

-- | All of the teams in the NFC
nfc :: [TeamOrMultiple]
nfc =
  concat
    [ nfcNorth,
      nfcEast,
      nfcSouth,
      nfcWest
    ]

-- | All of the teams in the AFC
afc :: [TeamOrMultiple]
afc =
  concat
    [ afcNorth,
      afcEast,
      afcSouth,
      afcWest
    ]

-- * League

-- | All of the teams in the NFL
all32Teams :: [TeamOrMultiple]
all32Teams = nfc ++ afc

-- | All of the teams in the NFL plus retired teams
all32TeamsPlusLegends :: [TeamOrMultiple]
all32TeamsPlusLegends = withLegends all32Teams

-- * Other bits

-- | The theme teams I would rather make
preferences :: [Team]
preferences = [legends, titans, seahawks, eagles, raiders]

-- | Quick function to add legends to a given list of teams
withLegends :: [TeamOrMultiple] -> [TeamOrMultiple]
withLegends ts = Team legends : ts

-- | Some special designations for particular players for whom otherwise
-- the JSON file would be interminably long
specialTeamDesignations :: [(EncodedTeamOrMultiple, [TeamOrMultiple])]
specialTeamDesignations =
  [ ("all32Teams", all32Teams),
    ("all32TeamsPlusLegends", all32TeamsPlusLegends),
    ("gronkTeams", teamsForSlots 2 all32TeamsPlusLegends)
  ]

encodeTeamOrMultiples :: [TeamOrMultiple] -> [EncodedTeamOrMultiple]
encodeTeamOrMultiples toms = case find ((== toms) . snd) specialTeamDesignations of
  Just (enc, _) -> [enc]
  Nothing -> map encodeTeamOrMultiple toms

-- | TeamOrMultiple - a straightforward one of encoding as a simple string
encodeTeamOrMultiple :: TeamOrMultiple -> EncodedTeamOrMultiple
encodeTeamOrMultiple NoTeam = ""
encodeTeamOrMultiple (Team t) = t
encodeTeamOrMultiple (MultipleTeam t n) = t ++ "." ++ show n
encodeTeamOrMultiple (Teams ts) = intercalate "|" . map encodeTeamOrMultiple $ ts

decodeTeamOrMultiples :: [EncodedTeamOrMultiple] -> [TeamOrMultiple]
decodeTeamOrMultiples etoms = case find ((== head etoms) . fst) specialTeamDesignations of
  Just (_, toms) -> toms
  Nothing -> map decodeTeamOrMultiple etoms

-- | Converting a given Team to a TeamOrMultiple
decodeTeamOrMultiple :: EncodedTeamOrMultiple -> TeamOrMultiple
decodeTeamOrMultiple s
  | '|' `elem` s = Teams $ map decodeTeamOrMultiple . splitOn "|" $ s
  | '.' `elem` s =
    let (teamName, '.' : num) = break (== '.') s
     in MultipleTeam teamName (read num :: Int)
  | otherwise = Team s

