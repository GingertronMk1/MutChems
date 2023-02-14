-- | Module: Data.Teams
--
-- All the teams
module Data.Teams where

import Data.List
import Functions.Application
import Types.Basic
import Types.Team
import Types.TeamOrMultiple

-- * Divisions

-- | Convert a list of Teams to a list of TeamOrMultiples
teamsToTeamOrMultiples :: [TeamData] -> [TeamOrMultiple]
teamsToTeamOrMultiples = map Team

-- | All of the teams in the NFC North
nfcNorth :: [TeamData]
nfcNorth = [Bears, Lions, Packers, Vikings]

-- | All of the teams in the nNorth as TeamOrMultiples
nfcNorthAsTeamOrMultiples :: [TeamOrMultiple]
nfcNorthAsTeamOrMultiples = teamsToTeamOrMultiples nfcNorth

-- | All of the teams in the NFC East
nfcEast :: [TeamData]
nfcEast = [Commanders, Cowboys, Giants, Eagles]

-- | All of the teams in the nEast as TeamOrMultiples
nfcEastAsTeamOrMultiples :: [TeamOrMultiple]
nfcEastAsTeamOrMultiples = teamsToTeamOrMultiples nfcEast

-- | All of the teams in the NFC South
nfcSouth :: [TeamData]
nfcSouth = [Buccaneers, Falcons, Panthers, Saints]

-- | All of the teams in the nSouth as TeamOrMultiples
nfcSouthAsTeamOrMultiples :: [TeamOrMultiple]
nfcSouthAsTeamOrMultiples = teamsToTeamOrMultiples nfcSouth

-- | All of the teams in the NFC West
nfcWest :: [TeamData]
nfcWest = [Cardinals, Niners, Rams, Seahawks]

-- | All of the teams in the nWest as TeamOrMultiples
nfcWestAsTeamOrMultiples :: [TeamOrMultiple]
nfcWestAsTeamOrMultiples = teamsToTeamOrMultiples nfcWest

-- | All of the teams in the AFC North
afcNorth :: [TeamData]
afcNorth = [Bengals, Browns, Ravens, Steelers]

-- | All of the teams in the aNorth as TeamOrMultiples
afcNorthAsTeamOrMultiples :: [TeamOrMultiple]
afcNorthAsTeamOrMultiples = teamsToTeamOrMultiples afcNorth

-- | All of the teams in the AFC East
afcEast :: [TeamData]
afcEast = [Bills, Dolphins, Jets, Patriots]

-- | All of the teams in the aEast as TeamOrMultiples
afcEastAsTeamOrMultiples :: [TeamOrMultiple]
afcEastAsTeamOrMultiples = teamsToTeamOrMultiples afcEast

-- | All of the teams in the AFC South
afcSouth :: [TeamData]
afcSouth = [Colts, Jaguars, Texans, Titans]

-- | All of the teams in the aSouth as TeamOrMultiples
afcSouthAsTeamOrMultiples :: [TeamOrMultiple]
afcSouthAsTeamOrMultiples = teamsToTeamOrMultiples afcSouth

-- | All of the teams in the AFC West
afcWest :: [TeamData]
afcWest = [Broncos, Chargers, Chiefs, Raiders]

-- | All of the teams in the aWest as TeamOrMultiples
afcWestAsTeamOrMultiples :: [TeamOrMultiple]
afcWestAsTeamOrMultiples = teamsToTeamOrMultiples afcWest

-- * Conferences

-- | All of the teams in the NFC
nfc :: [TeamData]
nfc =
  concat
    [ nfcNorth,
      nfcEast,
      nfcSouth,
      nfcWest
    ]

-- | All of the teams in the NFC as TeamOrMultiples
nfcAsTeamOrMultiples :: [TeamOrMultiple]
nfcAsTeamOrMultiples = teamsToTeamOrMultiples nfc

-- | All of the teams in the AFC
afc :: [TeamData]
afc =
  concat
    [ afcNorth,
      afcEast,
      afcSouth,
      afcWest
    ]

-- | All of the teams in the AFC as TeamOrMultiples
afcAsTeamOrMultiples :: [TeamOrMultiple]
afcAsTeamOrMultiples = teamsToTeamOrMultiples afc

-- * League

-- | All of the teams in the NFL
all32Teams :: [TeamData]
all32Teams = nfc ++ afc

-- | All of the teams in the NFL as TeamOrMultiples
all32TeamsAsTeamOrMultiples :: [TeamOrMultiple]
all32TeamsAsTeamOrMultiples = teamsToTeamOrMultiples all32Teams

-- | All of the teams in the NFL plus retired teams
all32TeamsPlusLegends :: [TeamData]
all32TeamsPlusLegends = Legends : all32Teams

-- | All of the teams in the NFL plus retired teams as TeamOrMultiples
all32TeamsPlusLegendsAsTeamOrMultiples :: [TeamOrMultiple]
all32TeamsPlusLegendsAsTeamOrMultiples = teamsToTeamOrMultiples all32TeamsPlusLegends

-- | The teams available for Rob Gronkowski's special S3 card
gronkTeams :: [TeamOrMultiple]
gronkTeams = teamsForSlots 2 all32TeamsPlusLegendsAsTeamOrMultiples

-- * Other bits

-- | Some special designations for particular players for whom otherwise
-- the JSON file would be interminably long
specialTeamDesignations :: [(EncodedTeamOrMultiple, [TeamOrMultiple])]
specialTeamDesignations =
  [ ("all32Teams", all32TeamsAsTeamOrMultiples),
    ("all32TeamsPlusLegends", all32TeamsPlusLegendsAsTeamOrMultiples),
    ("gronkTeams", gronkTeams)
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
     in MultipleTeam (read teamName :: TeamData) (read num :: Int)
  | otherwise = Team (read s :: TeamData)

-- | Encode a list of TeamOrMultiples
encodeTeamOrMultiples :: [TeamOrMultiple] -> [EncodedTeamOrMultiple]
encodeTeamOrMultiples toms = case find ((== toms) . snd) specialTeamDesignations of
  Just (enc, _) -> [enc]
  Nothing -> map encodeTeamOrMultiple toms

-- | TeamOrMultiple - a straightforward one of encoding as a simple string
encodeTeamOrMultiple :: TeamOrMultiple -> EncodedTeamOrMultiple
encodeTeamOrMultiple NoTeam = ""
encodeTeamOrMultiple (Team t) = show t
encodeTeamOrMultiple (MultipleTeam t n) = show t ++ "." ++ show n
encodeTeamOrMultiple (Teams ts) = intercalate "|" . map encodeTeamOrMultiple $ ts
