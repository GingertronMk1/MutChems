-- | Module: Data.Teams
--
-- All the teams
module Data.Teams where

import Data.List
import Functions.Application
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

teamsToTeamOrMultiples :: [Team] -> [TeamOrMultiple]
teamsToTeamOrMultiples = map Team

-- | All of the teams in the NFCAsTeamOrMultiples North
nfcNorth :: [Team]
nfcNorth = [bears, lions, packers, vikings]

nfcNorthAsTeamOrMultiples :: [TeamOrMultiple]
nfcNorthAsTeamOrMultiples = teamsToTeamOrMultiples nfcNorth

-- | All of the teams in the NFCAsTeamOrMultiples East
nfcEast :: [Team]
nfcEast = [commanders, cowboys, giants, eagles]

nfcEastAsTeamOrMultiples :: [TeamOrMultiple]
nfcEastAsTeamOrMultiples = teamsToTeamOrMultiples nfcEast

-- | All of the teams in the NFCAsTeamOrMultiples South
nfcSouth :: [Team]
nfcSouth = [buccaneers, falcons, panthers, saints]

nfcSouthAsTeamOrMultiples :: [TeamOrMultiple]
nfcSouthAsTeamOrMultiples = teamsToTeamOrMultiples nfcSouth

-- | All of the teams in the NFCAsTeamOrMultiples West
nfcWest :: [Team]
nfcWest = [cardinals, niners, rams, seahawks]

nfcWestAsTeamOrMultiples :: [TeamOrMultiple]
nfcWestAsTeamOrMultiples = teamsToTeamOrMultiples nfcWest

-- | All of the teams in the AFCAsTeamOrMultiples North
afcNorth :: [Team]
afcNorth = [bengals, browns, ravens, steelers]

afcNorthAsTeamOrMultiples :: [TeamOrMultiple]
afcNorthAsTeamOrMultiples = teamsToTeamOrMultiples afcNorth

-- | All of the teams in the AFCAsTeamOrMultiples East
afcEast :: [Team]
afcEast = [bills, dolphins, jets, patriots]

afcEastAsTeamOrMultiples :: [TeamOrMultiple]
afcEastAsTeamOrMultiples = teamsToTeamOrMultiples afcEast

-- | All of the teams in the AFCAsTeamOrMultiples South
afcSouth :: [Team]
afcSouth = [colts, jaguars, texans, titans]

afcSouthAsTeamOrMultiples :: [TeamOrMultiple]
afcSouthAsTeamOrMultiples = teamsToTeamOrMultiples afcSouth

-- | All of the teams in the AFCAsTeamOrMultiples West
afcWest :: [Team]
afcWest = [broncos, chargers, chiefs, raiders]

afcWestAsTeamOrMultiples :: [TeamOrMultiple]
afcWestAsTeamOrMultiples = teamsToTeamOrMultiples afcWest

-- * Conferences

nfc :: [Team]
nfc =
  concat
    [ nfcNorth,
      nfcEast,
      nfcSouth,
      nfcWest
    ]

-- | All of the teams in the NFCAsTeamOrMultiples
nfcAsTeamOrMultiples :: [TeamOrMultiple]
nfcAsTeamOrMultiples = teamsToTeamOrMultiples nfc

afc :: [Team]
afc =
  concat
    [ afcNorth,
      afcEast,
      afcSouth,
      afcWest
    ]

-- | All of the teams in the AFCAsTeamOrMultiples
afcAsTeamOrMultiples :: [TeamOrMultiple]
afcAsTeamOrMultiples = teamsToTeamOrMultiples afc

-- * League

all32Teams :: [Team]
all32Teams = nfc ++ afc

-- | All of the teams in the NFL
all32TeamsAsTeamOrMultiples :: [TeamOrMultiple]
all32TeamsAsTeamOrMultiples = teamsToTeamOrMultiples all32Teams

all32TeamsPlusLegends :: [Team]
all32TeamsPlusLegends = legends : all32Teams

-- | All of the teams in the NFL plus retired teams
all32TeamsPlusLegendsAsTeamOrMultiples :: [TeamOrMultiple]
all32TeamsPlusLegendsAsTeamOrMultiples = teamsToTeamOrMultiples all32TeamsPlusLegends

-- * Other bits

-- | Some special designations for particular players for whom otherwise
-- the JSON file would be interminably long
specialTeamDesignations :: [(EncodedTeamOrMultiple, [TeamOrMultiple])]
specialTeamDesignations =
  [ ("all32Teams", all32TeamsAsTeamOrMultiples),
    ("all32TeamsPlusLegends", all32TeamsPlusLegendsAsTeamOrMultiples),
    ("gronkTeams", teamsForSlots 2 all32TeamsPlusLegendsAsTeamOrMultiples)
  ]

-- | Decode a given String into its TeamOrMultiple
decodeTeamOrMultiples :: [EncodedTeamOrMultiple] -> [TeamOrMultiple]
decodeTeamOrMultiples etoms = case lookup (head etoms) specialTeamDesignations of
  Just toms -> toms
  Nothing -> map decodeTeamOrMultiple etoms

-- | Converting a given Team to a TeamOrMultiple
decodeTeamOrMultiple :: EncodedTeamOrMultiple -> TeamOrMultiple
decodeTeamOrMultiple s
  | '|' `elem` s = Teams $ map decodeTeamOrMultiple . splitOn (== '|') $ s
  | '.' `elem` s =
    let (teamName, '.' : num) = break (== '.') s
     in MultipleTeam teamName (read num :: Int)
  | otherwise = Team s

-- | Encode a list of TeamOrMultiples
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
