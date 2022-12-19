-- |
-- Module: Data.Squad
module Data.Squad where

import           Data.Positions
import           Data.Teams
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | Base squad.
baseSquad :: LineupWithPositions
baseSquad = [
    (qb,[
      ("Ryan Fitzpatrick", [Team bengals, Team bills, Team buccaneers, Team commanders, Team dolphins, Team jets, Team legends, Team rams, Team texans, Team titans]),
      ("Tim Tebow", [Team legends, Team broncos, Team jets])
    ]),
    (hb,[
      ("CJ2K", [Team legends, Team titans, Team jets, Team cardinals]),
      ("Rod Woodson (HB)", [Team legends, Team raiders, Team niners, Team steelers, Team ravens]),
      ("Travis Etienne Jr", [Team jaguars])
    ]),
    (fb,[
      ("Nightmare", [Team legends, Team chiefs]),
      ("Jim Taylor", [Team legends, Team packers, Team saints])
    ]),
    (te,[
      ("Dave Casper", all32TeamsPlusLegends),
      ("David Njoku", [Team browns]),
      ("Hayden Hurst", [Team ravens, Team falcons, Team bengals])
    ]),
    (wr,[
      ("Calvin Johnson", [Team lions, Team legends]),
      ("Randy Moss", [Team legends, Team titans, Team raiders, Team niners, Team vikings, Team patriots]),
      ("Devin Hester", [Team legends, Team ravens, Team falcons, Team seahawks, Team bears]),
      ("Deion Sanders (WR)", [ Team legends, Team ravens, Team niners, Team falcons, Team commanders, Team cowboys]),
      ("Charlie Joiner", [Team legends, Team bengals, Team chargers, Team titans])
    ]),
    (lt,[
      ("Jonathan Ogden", [Team legends, Team ravens]),
      ("Anthony Munoz", [Team legends, Team bengals])
    ]),
    (lg,[
      ("Quenton Nelson", [Team colts]),
      ("Alan Faneca", [Team legends, Team steelers, Team jets, Team cardinals])
    ]),
    (c,[
      ("Russ Grimm", [Team legends, Team commanders]),
      ("Creed Humphrey", [Team chiefs])
    ]),
    (rg,[
      ("Larry Allen", [Team legends, Team niners, Team cowboys]),
      ("Will Shields", [Team legends, Team chiefs])
    ]),
    (rt,[
      ("Willie Anderson", [Team legends, Team bengals, Team ravens]),
      ("Gary Zimmerman", [Team legends, Team broncos, Team vikings])
    ]),
    (mlb,[
      ("Devin Bush", [Team steelers]),
      ("Bobby Wagner", [Team seahawks, Team rams]),
      ("Ted Hendricks", [Team colts, Team packers, Team raiders, Team legends]),
      ("Kiko Alonso", [Team legends, Team bills, Team dolphins, Team eagles, Team saints])
    ]),
    (rolb,[
      ("Khalil Mack", [Team raiders, Team bears, Team chargers]),
      ("Jonathan Casillas", [Team legends, Team buccaneers, Team giants, Team patriots, Team saints])
    ]),
    (lolb,[
      ("Carl Banks", [Team legends, Team browns, Team commanders, Team giants]),
      ("Bruce Irvin", [Team seahawks, Team bears, Team falcons, Team panthers, Team raiders])
    ]),
    (dt,[
      ("Richard Seymour", [Team legends, Team patriots, Team raiders]),
      ("Sam Adams", all32TeamsPlusLegends),
      ("Tony Siragusa", [Team legends, Team ravens, Team colts]),
      ("Merlin Olsen", [Team legends, Team rams])
    ]),
    (le,[
      ("Jevon Kearse", [Team legends, Team titans, Team eagles]),
      ("George Karlaftis", [Team broncos, Team buccaneers, Team cardinals, Team chiefs, Team giants, Team jaguars, Team niners, Team packers, Team saints, Team seahawks, Team vikings])
    ]),
    (re,[
      ("Cameron Wake", [Team legends, Team titans, Team dolphins]),
      ("Bruce Smith", [Team legends, Team bills, Team commanders])
    ]),
    (ss,[
      ("John Lynch", [Team legends, Team buccaneers, Team broncos]),
      ("Nolan Cromwell", [Team legends, Team rams])
    ]),
    (fs,[
      ("Sean Taylor", [Team legends, Team commanders]),
      ("Rod Woodson", [Team legends, Team niners, Team steelers, Team raiders, Team ravens])
    ]),
    (cb,[
      ("Deion Sanders", all32TeamsPlusLegends),
      ("Benjamin St-Juste", [Team commanders]),
      ("Samari Rolle", [Team legends, Team ravens, MultipleTeam titans 2]),
      ("Adoree Jackson", [Team giants, Team titans]),
      ("Champ Bailey", [Team commanders, Team broncos, Team legends])
    ]),
    (k,[
      ("Adam Vinatieri", [Team legends, Team colts, Team patriots])
    ]),
   (p,[
      ("Ray Guy", [Team legends, Team raiders])
    ])
  ]

-- | Team affinity strategy card.
strategy :: [TeamOrMultiple]
strategy = []

-- | Players I'm looking into.
prospectiveAdditions :: [ProspectiveChange]
prospectiveAdditions = [
    -- Zero chill forge
    Replacement "Hayden Hurst" ("Donald Parham", [Team chargers]),
    -- Campus Heroes to round out the legends
    Replacement "Travis Etienne Jr" ("Demarco Murray", [Team legends, Team cowboys, Team eagles, Team titans]),
    Replacement "Ted Hendricks" ("Junior Seau", [Team legends, Team chargers, Team dolphins, Team patriots]),
    Replacement "Bruce Irvin" ("Ted Hendricks", [Team colts, Team packers, Team raiders, Team legends]),
    Replacement "Bobby Wagner" ("Derrick Thompson", [Team legends, Team chiefs])
  ]

