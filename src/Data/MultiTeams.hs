-- |
-- Module: Data.Squad

module Data.MultiTeams where

import Data.Teams
import Types.TeamOrMultiple

nfcNorth :: [TeamOrMultiple]
nfcNorth = [Team packers, Team lions, Team vikings, Team bears]

nfcEast :: [TeamOrMultiple]
nfcEast = [Team cowboys, Team giants, Team commanders, Team eagles]

nfcSouth :: [TeamOrMultiple]
nfcSouth = [Team saints, Team buccaneers, Team panthers, Team falcons] 

nfcWest :: [TeamOrMultiple]
nfcWest = [Team seahawks, Team cardinals, Team niners, Team rams] 

afcNorth :: [TeamOrMultiple]
afcNorth = [Team steelers, Team bengals, Team browns, Team ravens] 

afcEast :: [TeamOrMultiple]
afcEast = [Team dolphins, Team bills, Team jets, Team patriots] 

afcSouth :: [TeamOrMultiple]
afcSouth = [Team colts, Team jaguars, Team texans, Team titans] 

afcWest :: [TeamOrMultiple]
afcWest = [Team chiefs, Team chargers, Team raiders, Team broncos] 

nfc :: [TeamOrMultiple]
nfc = concat [
    nfcNorth,
    nfcEast,
    nfcSouth,
    nfcWest
  ]

afc :: [TeamOrMultiple]
afc = concat [
    afcNorth,
    afcEast,
    afcSouth,
    afcWest
  ]

all32Teams :: [TeamOrMultiple]
all32Teams = nfc ++ afc

all32TeamsPlusLegends :: [TeamOrMultiple]
all32TeamsPlusLegends = Team legends : all32Teams