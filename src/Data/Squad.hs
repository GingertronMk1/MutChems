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
              },
            emptyPlayer
              { pName = "Travis Etienne Jr",
                pTeams =
                  [ Team jaguars
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.fb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Nightmare",
                pTeams =
                  [ Team legends,
                    Team chiefs
                  ]
              },
            emptyPlayer
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
              { pName = "Dave Casper",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "David Njoku",
                pTeams =
                  [ Team browns
                  ]
              },
            emptyPlayer
              { pName = "Donald Parham",
                pTeams =
                  [ Team chargers
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.wr,
        pgPlayers =
          [ emptyPlayer
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
              { pName = "Devin Hester",
                pTeams =
                  [ Team legends,
                    Team ravens,
                    Team falcons,
                    Team seahawks,
                    Team bears
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
              },
            emptyPlayer
              { pName = "Charlie Joiner",
                pTeams =
                  [ Team legends,
                    Team bengals,
                    Team chargers,
                    Team titans
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
              { pName = "Quenton Nelson",
                pTeams =
                  [ Team colts
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
              },
            emptyPlayer
              { pName = "Creed Humphrey",
                pTeams =
                  [ Team chiefs
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
              { pName = "Devin Bush",
                pTeams =
                  [ Team steelers
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
              { pName = "Khalil Mack",
                pTeams =
                  [ Team raiders,
                    Team bears,
                    Team chargers
                  ]
              },
            emptyPlayer
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
              { pName = "Ted Hendricks (SS)",
                pTeams =
                  [ Team legends,
                    Team raiders,
                    Team colts,
                    Team packers
                  ]
              },
            emptyPlayer
              { pName = "Nolan Cromwell",
                pTeams =
                  [ Team legends,
                    Team rams
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
              { pName = "Rod Woodson",
                pTeams =
                  [ Team legends,
                    Team steelers,
                    Team raiders,
                    Team ravens,
                    Team niners
                  ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.cb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Deion Sanders",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "Benjamin St-Juste",
                pTeams =
                  [ Team commanders
                  ]
              },
            emptyPlayer
              { pName = "Samari Rolle",
                pTeams =
                  [ Team legends,
                    Team ravens,
                    MultipleTeam titans 2
                  ]
              },
            emptyPlayer
              { pName = "Calvin Johnson (CB)",
                pTeams =
                  [ Team legends,
                    Team lions
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
  [ Replacement -- Zero chill forge
      "Rod Woodson"
      emptyPlayer
        { pName = "Weapon X",
          pTeams =
            [ Team legends,
              Team eagles,
              Team broncos
            ]
        },
    Replacement
      "Benjamin St-Juste"
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
    Replacement
      "Deion Sanders (WR)"
      emptyPlayer
        { pName = "Steve Smith Sr",
          pTeams =
            [ Team legends,
              Team panthers,
              Team ravens
            ]
        }
  ]
