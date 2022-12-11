-- |
-- Module: Data.Squad
module Data.Squad where

import           Data.Positions
import           Data.Teams
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | Base squad.
baseSquad :: LineupWithPositions
baseSquad =
  [
    (qb,[
      ("Air McNair", [Team legends, Team titans, Team ravens]),
      ("Christian McCaffrey (QB)", [Team niners, Team panthers])
    ]),
    (hb,[
      ("CJ2K", [Team legends, Team titans, Team jets, Team cardinals]),
      ("Rod Woodson (HB)", [Team legends, Team raiders, Team niners, Team steelers, Team ravens]),
      ("Travis Etienne Jr", [Team jaguars])
    ]),
    (fb,[
      ("Patrick Ricard", [Team ravens]),
      ("Khari Blasingame", [Team titans, Team bears])
    ]),
    (te,[
      ("Dave Casper", all32TeamsPlusLegends),
      ("David Njoku", [Team browns]),
      ("Hayden Hurst", [Team ravens, Team falcons, Team bengals])
    ]),
    (wr,[
      ("Randy Moss", [Team legends, Team titans, Team raiders, Team niners, Team vikings, Team patriots]),
      ("AJ Brown", [Team eagles, Team titans]),
      ("Charlie Joiner", [Team legends, Team bengals, Team chargers, Team titans]),
      ("Plaxico Burress", [Team legends, Team giants, Team jets, Team steelers]),
      ("Devin Hester", [
            Team legends,
            Team ravens,
            Team falcons,
            Team seahawks,
            Team bears
          ])
    ]),
    (lt,[
      ("Joe Thomas", [Team browns]),
      ("Charles Cross", [Team seahawks])
    ]),
    (lg,[
      ("Quenton Nelson", [Team colts]),
      ("Andrew Norwell", [Team commanders, Team jaguars, Team panthers])
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
      ("Tristan Wirfs", [Team buccaneers]),
      ("Tom Compton", [Team niners, Team bears, Team broncos, Team commanders, Team falcons, Team jets, Team vikings])
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
      ("Julius Peppers", [Team legends, Team panthers, Team packers, Team bears]),
      ("George Karlaftis", [
        Team chiefs,
        Team vikings,
        Team saints,
        Team giants,
        Team packers,
        Team broncos,
        Team jaguars,
        Team niners,
        Team cardinals,
        Team seahawks,
        Team buccaneers
      ])
    ]),
    (re,[
      ("Aaron Donald", [Team rams]),
      ("Aidan Hutchinson", [Team lions])
    ]),
    (ss,[
      ("Isaiah Pola-Mao", [Team raiders]),
      ("Talanoa Hufanga", [Team niners])
    ]),
    (fs,[
      ("Malik Hooker", [Team colts, Team cowboys]),
      ("Eddie Jackson", [Team bears])
    ]),
    (cb,[
      ("Deion Sanders", all32TeamsPlusLegends),
      ("Benjamin St Juste", [Team commanders]),
      ("Champ Bailey", [Team commanders, Team broncos, Team legends]),
      ("Shaquill Griffin", [Team seahawks, Team jaguars]),
      ("Sauce Gardner", [Team jets])
    ]),
    (k,[
      ("Graham Gano", [Team giants, Team panthers, Team commanders])
    ]),
   (p,[
      ("Ray Guy", [Team legends, Team raiders])
    ])
  ]

-- | Team affinity strategy card.
strategy :: [TeamOrMultiple]
strategy = [
  MultipleTeam titans 2,
  MultipleTeam titans 5
  ]

-- | Players I'm looking into.
prospectiveAdditions :: [ProspectiveChange]
prospectiveAdditions =
  [
    Replacement "Air McNair" ("Ryan Fitzpatrick", [
      Team legends,
      Team bengals,
      Team bills,
      Team buccaneers,
      Team commanders,
      Team dolphins,
      Team jets,
      Team rams,
      Team texans,
      Team titans
    ]),
    Replacement "Sauce Gardner" ("Adoree Jackson", [
      Team giants,
      Team titans
    ]),
    Replacement "Hayden Hurst" ("Donald Parham", [Team chargers]),
    Replacement "Plaxico Burress" ("Deion Sanders (WR)", [
      Team legends,
      Team ravens,
      Team niners,
      Team falcons,
      Team commanders,
      Team cowboys
    ])
  ]

