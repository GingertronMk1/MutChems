-- |
-- Module: Data.Squad
module Data.Squad where

import qualified Data.Positions as Positions
import Data.Teams
import Types.ProspectiveChange
import Types.TeamOrMultiple

-- | Base squad.
baseSquad :: InitialLineup
baseSquad =
  [ PositionGroup
      { pgPosition = Positions.qb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Ryan Fitzpatrick",
                pTeams =
                  [ Team bengals,
                    Team bills,
                    Team buccaneers,
                    Team commanders,
                    Team dolphins,
                    Team jets,
                    Team legends,
                    Team rams,
                    Team texans,
                    Team titans
                  ]
              },
            emptyPlayer
              { pName = "Tim Tebow",
                pTeams =
                  [ Team legends,
                    Team broncos,
                    Team jets
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.hb,
        pgPlayers =
          [ emptyPlayer
              { pName = "LaDainian Tomlinson",
                pTeams =
                  [ Team legends,
                    Team chargers,
                    Team jets
                  ]
              },
            emptyPlayer
              { pName = "CJ2K",
                pTeams =
                  [ Team legends,
                    Team jets,
                    Team cardinals
                  ]
              },
            emptyPlayer
              { pName = "Rod Woodson (HB)",
                pTeams =
                  [ Team legends,
                    Team steelers,
                    Team ravens,
                    Team niners,
                    Team raiders
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.fb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Jim Taylor",
                pTeams =
                  [ Team legends,
                    Team packers,
                    Team saints
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.te,
        pgPlayers =
          [ emptyPlayer
              { pName = "Donald Parham",
                pTeams =
                  [ Team chargers
                  ]
              },
            emptyPlayer
              { pName = "Dave Casper",
                pTeams = all32TeamsPlusLegends
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.wr,
        pgPlayers =
          [ emptyPlayer
              { pName = "Herman Moore",
                pTeams =
                  [ Team legends,
                    Team lions,
                    Team giants
                  ]
              },
            emptyPlayer
              { pName = "Calvin Johnson",
                pTeams =
                  [ Team lions,
                    Team legends
                  ]
              },
            emptyPlayer
              { pName = "Randy Moss",
                pTeams =
                  [ Team legends,
                    Team titans,
                    Team raiders,
                    Team niners,
                    Team vikings,
                    Team patriots
                  ]
              },
            emptyPlayer
              { pName = "Deion Sanders (WR)",
                pTeams =
                  [ Team legends,
                    Team ravens,
                    Team niners,
                    Team falcons,
                    Team commanders,
                    Team cowboys
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.lt,
        pgPlayers =
          [ emptyPlayer
              { pName = "Jonathan Ogden",
                pTeams =
                  [ Team legends,
                    Team ravens
                  ]
              },
            emptyPlayer
              { pName = "Anthony Munoz",
                pTeams =
                  [ Team legends,
                    Team bengals
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.lg,
        pgPlayers =
          [ emptyPlayer
              { pName = "Randall McDaniel",
                pTeams =
                  [ Team legends,
                    Team vikings,
                    Team buccaneers
                  ]
              },
            emptyPlayer
              { pName = "Alan Faneca",
                pTeams =
                  [ Team legends,
                    Team steelers,
                    Team jets,
                    Team cardinals
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.c,
        pgPlayers =
          [ emptyPlayer
              { pName = "Russ Grimm",
                pTeams =
                  [ Team legends,
                    Team commanders
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.rg,
        pgPlayers =
          [ emptyPlayer
              { pName = "Larry Allen",
                pTeams =
                  [ Team legends,
                    Team niners,
                    Team cowboys
                  ]
              },
            emptyPlayer
              { pName = "Will Shields",
                pTeams =
                  [ Team legends,
                    Team chiefs
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.rt,
        pgPlayers =
          [ emptyPlayer
              { pName = "Willie Anderson",
                pTeams =
                  [ Team legends,
                    Team bengals,
                    Team ravens
                  ]
              },
            emptyPlayer
              { pName = "Gary Zimmerman",
                pTeams =
                  [ Team legends,
                    Team broncos,
                    Team vikings
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.mlb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Ray Nitschke",
                pTeams =
                  [ Team legends,
                    Team packers
                  ]
              },
            emptyPlayer
              { pName = "Derrick Thompson",
                pTeams =
                  [ Team legends,
                    Team chiefs,
                    Team raiders
                  ]
              },
            emptyPlayer
              { pName = "Junior Seau",
                pTeams =
                  [ Team legends,
                    Team chargers,
                    Team dolphins,
                    Team patriots
                  ]
              },
            emptyPlayer
              { pName = "Kiko Alonso",
                pTeams =
                  [ Team legends,
                    Team bills,
                    Team dolphins,
                    Team eagles,
                    Team saints
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.rolb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Jonathan Casillas",
                pTeams =
                  [ Team legends,
                    Team buccaneers,
                    Team giants,
                    Team patriots,
                    Team saints
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.lolb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Mike Vrabel",
                pTeams =
                  [ Team legends,
                    Team patriots,
                    Team steelers,
                    Team chiefs
                  ]
              },
            emptyPlayer
              { pName = "Carl Banks",
                pTeams =
                  [ Team legends,
                    Team browns,
                    Team commanders,
                    Team giants
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.dt,
        pgPlayers =
          [ emptyPlayer
              { pName = "Richard Seymour",
                pTeams =
                  [ Team legends,
                    Team patriots,
                    Team raiders
                  ]
              },
            emptyPlayer
              { pName = "Sam Adams",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "Tony Siragusa",
                pTeams =
                  [ Team legends,
                    Team ravens,
                    Team colts
                  ]
              },
            emptyPlayer
              { pName = "Merlin Olsen",
                pTeams =
                  [ Team legends,
                    Team rams
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.le,
        pgPlayers =
          [ emptyPlayer
              { pName = "Jevon Kearse",
                pTeams =
                  [ Team legends,
                    Team titans,
                    Team eagles
                  ]
              },
            emptyPlayer
              { pName = "George Karlaftis",
                pTeams =
                  [ Team broncos,
                    Team buccaneers,
                    Team cardinals,
                    Team chiefs,
                    Team giants,
                    Team jaguars,
                    Team niners,
                    Team packers,
                    Team saints,
                    Team seahawks,
                    Team vikings
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.re,
        pgPlayers =
          [ emptyPlayer
              { pName = "Bruce Smith",
                pTeams =
                  [ Team legends,
                    Team bills,
                    Team commanders
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.ss,
        pgPlayers =
          [ emptyPlayer
              { pName = "Kam Chancellor",
                pTeams =
                  [ Team legends,
                    Team seahawks
                  ]
              },
            emptyPlayer
              { pName = "Adrian Wilson",
                pTeams =
                  [ Team legends,
                    Team cardinals
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.fs,
        pgPlayers =
          [ emptyPlayer
              { pName = "Sean Taylor",
                pTeams =
                  [ Team legends,
                    Team commanders
                  ]
              },
            emptyPlayer
              { pName = "Steve Atwater",
                pTeams =
                  [ Team legends,
                    Team broncos,
                    Team jets
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.cb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Calvin Johnson (CB)",
                pTeams =
                  [ Team legends,
                    Team lions
                  ]
              },
            emptyPlayer
              { pName = "Deion Sanders",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "Rod Woodson",
                pTeams =
                  [ Team legends,
                    Team steelers,
                    Team niners,
                    Team ravens,
                    Team raiders
                  ]
              },
            emptyPlayer
              { pName = "Mike Haynes",
                pTeams =
                  [ Team legends,
                    Team raiders,
                    Team patriots
                  ]
              },
            emptyPlayer
              { pName = "Champ Bailey",
                pTeams =
                  [ Team commanders,
                    Team broncos,
                    Team legends
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.k,
        pgPlayers =
          [ emptyPlayer
              { pName = "Adam Vinatieri",
                pTeams =
                  [ Team legends,
                    Team colts,
                    Team patriots
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.p,
        pgPlayers =
          [ emptyPlayer
              { pName = "Ray Guy",
                pTeams =
                  [ Team legends,
                    Team raiders
                  ]
              }
          ]
      }
  ]

-- | Team affinity strategy card.
strategy :: [TeamOrMultiple]
strategy = []

-- | Players I'm looking into.
prospectiveAdditions :: [ProspectiveChange]
prospectiveAdditions =
  [ Addition
      emptyPlayer
        { pName = "Derrick Brooks",
          pTeams =
            [ Team legends,
              Team buccaneers
            ],
          pPosition = Positions.rolb
        },
    Replacement
      "Jim Taylor"
      emptyPlayer
        { pName = "William Perry (FB)",
          pTeams =
            [ Team legends,
              Team bears,
              Team eagles
            ]
        }
  ]
