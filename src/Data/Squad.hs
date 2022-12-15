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
      ("Patrick Ricard", [Team ravens]),
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
      ("Joe Thomas", [Team browns]),
      ("Charles Cross", [Team seahawks])
    ]),
    (lg,[
      ("Quenton Nelson", [Team colts]),
      ("Alan Faneca", [Team legends, Team steelers, Team jets])
    ]),
    (c,[
      ("Creed Humphrey", [Team chiefs]),
      ("Frank Ragnow", [Team lions])
    ]),
    (rg,[
      ("Kevin Zeitler", [Team bengals, Team browns, Team giants, Team ravens]),
      ("Mark Glowinski", [Team giants, Team colts, Team seahawks])
    ]),
    (rt,[
      ("Willie Anderson", [Team legends, Team bengals, Team ravens]),
      ("Gary Zimmerman", [Team legends, Team broncos, Team vikings])
      -- ("Tom Compton", [Team niners, Team bears, Team broncos, Team commanders, Team falcons, Team jets, Team vikings])
    ]),
    (mlb,[
      ("Devin Bush", [Team steelers]),
      ("Bobby Wagner", [Team seahawks, Team rams]),
      ("Roquan Smith", [Team bears, Team ravens]),
      ("Kiko Alonso", [Team legends, Team bills, Team dolphins, Team eagles, Team saints])
    ]),
    (rolb,[
      ("Khalil Mack", [Team raiders, Team bears, Team chargers]),
      ("Jonathan Casillas", [Team legends, Team buccaneers, Team giants, Team patriots, Team saints])
    ]),
    (lolb,[
      ("Ted Hendricks", [Team colts, Team packers, Team raiders, Team legends]),
      ("Bruce Irvin", [Team seahawks, Team bears, Team falcons, Team panthers, Team raiders])
    ]),
    (dt,[
      ("Richard Seymour", [Team legends, Team patriots, Team raiders]),
      ("Sam Adams", all32TeamsPlusLegends),
      ("Jordan Davis", [Team eagles]),
      ("Tony Siragusa", [Team legends, Team ravens, Team colts])
    ]),
    (le,[
      ("Jevon Kearse", [Team legends, Team titans, Team eagles]),
      ("George Karlaftis", [Team broncos, Team buccaneers, Team cardinals, Team chiefs, Team giants, Team jaguars, Team niners, Team packers, Team saints, Team seahawks, Team vikings])
    ]),
    (re,[
      ("Cameron Wake", [Team legends, Team titans, Team dolphins]),
      ("Aidan Hutchinson", [Team lions])
    ]),
    (ss,[
      ("John Lynch", [Team legends, Team buccaneers, Team broncos]),
      ("Isaiah Pola-Mao", [Team raiders])
    ]),
    (fs,[
      ("Kevin Byard", [Team titans]),
      ("Malik Hooker", [Team colts, Team cowboys])
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
    Replacement "Bruce Irvin" ("Carl Banks", [Team legends, Team browns, Team commanders, Team giants]),
    -- Zero chill forge
    Replacement "Hayden Hurst" ("Donald Parham", [Team chargers]),
    -- Auction
    Replacement "Patrick Ricard" ("Nightmare", [Team legends, Team chiefs]),
    -- Zero Chill field pass final reward
    Replacement "Malik Hooker" ("Sean Taylor", [Team legends, Team commanders]),
    -- Campus Heroes to round out the legends
    Replacement "Jordan Davis" ("Haloti Ngata", [Team legends, Team ravens])
  ]

