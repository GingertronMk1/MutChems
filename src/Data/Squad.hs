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
                  withLegends
                    [ Team bengals,
                      Team bills,
                      Team buccaneers,
                      Team commanders,
                      Team dolphins,
                      Team jets,
                      Team rams,
                      Team texans,
                      Team titans
                    ]
              },
            emptyPlayer
              { pName = "Tim Tebow",
                pTeams =
                  withLegends
                    [ Team broncos,
                      Team jets
                    ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.hb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Marshawn Lynch",
                pTeams =
                  withLegends
                    [ Team bills,
                      Team seahawks,
                      Team raiders
                    ]
              },
            emptyPlayer
              { pName = "CJ2K",
                pTeams =
                  withLegends
                    [ Team jets,
                      Team cardinals
                    ]
              },
            emptyPlayer
              { pName = "Rod Woodson (HB)",
                pTeams =
                  withLegends
                    [ Team steelers,
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
                  withLegends
                    [ Team packers,
                      Team saints
                    ]
              },
            emptyPlayer
              { pName = "Keith Byars",
                pTeams =
                  withLegends
                    [ MultipleTeam eagles 2,
                      Team dolphins,
                      Team patriots,
                      Team jets
                    ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.te,
        pgPlayers =
          [ emptyPlayer
              { pName = "Delanie Walker",
                pTeams =
                  withLegends
                    [ Team niners,
                      Team titans
                    ],
                pPosition = Positions.te
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
              { pName = "Calvin Johnson",
                pTeams =
                  withLegends
                    [ Team lions
                    ]
              },
            emptyPlayer
              { pName = "Steve Smith Sr",
                pTeams =
                  withLegends
                    [ Team ravens,
                      Team panthers
                    ]
              },
            emptyPlayer
              { pName = "Randy Moss",
                pTeams =
                  withLegends
                    [ Team titans,
                      Team raiders,
                      Team niners,
                      Team vikings,
                      Team patriots
                    ]
              },
            emptyPlayer
              { pName = "Deion Sanders (WR)",
                pTeams =
                  withLegends
                    [ Team ravens,
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
                  withLegends
                    [ Team ravens
                    ]
              }
          ]
      },
    PositionGroup
      { pgPosition = Positions.lg,
        pgPlayers =
          [ emptyPlayer
              { pName = "Alan Faneca",
                pTeams =
                  withLegends
                    [ Team steelers,
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
              { pName = "Jeff Saturday",
                pTeams =
                  withLegends
                    [ Team colts,
                      Team packers
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
                  withLegends
                    [ Team niners,
                      Team cowboys
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
                  withLegends
                    [ Team bengals,
                      Team ravens
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
                  withLegends
                    [ Team packers
                    ]
              },
            emptyPlayer
              { pName = "Derrick Johnson",
                pTeams =
                  withLegends
                    [ Team chiefs,
                      Team raiders
                    ]
              },
            emptyPlayer
              { pName = "Junior Seau",
                pTeams =
                  withLegends
                    [ Team chargers,
                      Team dolphins,
                      Team patriots
                    ]
              },
            emptyPlayer
              { pName = "Kiko Alonso",
                pTeams =
                  withLegends
                    [ Team bills,
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
                  withLegends
                    [ Team buccaneers,
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
              { pName = "Ted Hendricks",
                pTeams =
                  withLegends
                    [ Team packers,
                      Team colts
                    ]
              },
            emptyPlayer
              { pName = "Carl Banks",
                pTeams =
                  withLegends
                    [ Team browns,
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
                  withLegends
                    [ Team patriots,
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
                  withLegends
                    [ Team ravens,
                      Team colts
                    ]
              },
            emptyPlayer
              { pName = "Merlin Olsen",
                pTeams =
                  withLegends
                    [ Team rams
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
                  withLegends
                    [ Team titans,
                      Team eagles
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
                  withLegends
                    [ Team bills,
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
                  withLegends
                    [ Team seahawks
                    ]
              },
            emptyPlayer
              { pName = "Adrian Wilson",
                pTeams =
                  withLegends
                    [ Team cardinals
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
                  withLegends
                    [ Team commanders
                    ]
              },
            emptyPlayer
              { pName = "Steve Atwater",
                pTeams =
                  withLegends
                    [ Team broncos,
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
                  withLegends
                    [ Team lions
                    ]
              },
            emptyPlayer
              { pName = "Deion Sanders",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "Rod Woodson",
                pTeams =
                  withLegends
                    [ Team steelers,
                      Team niners,
                      Team ravens,
                      Team raiders
                    ]
              },
            emptyPlayer
              { pName = "Mike Haynes",
                pTeams =
                  withLegends
                    [ Team raiders,
                      Team patriots
                    ]
              },
            emptyPlayer
              { pName = "Champ Bailey",
                pTeams =
                  withLegends
                    [ Team commanders,
                      Team broncos
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
                  withLegends
                    [ Team colts,
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
                  withLegends
                    [ Team raiders
                    ]
              }
          ]
      }
  ]

-- | Team affinity strategy card.
strategy :: TeamOrMultiple
strategy = NoTeam

-- | Players I'm looking into.
prospectiveAdditions :: [ProspectiveChange]
prospectiveAdditions =
  [ Removals
      [ "Derrick Johnson",
        "Mike Haynes",
        "Ted Hendricks"
      ],
    Replacement
      "Adrian Wilson"
      emptyPlayer
        { pName = "Ted Hendricks",
          pTeams =
            withLegends
              [ Team raiders,
                Team packers,
                Team colts
              ]
        },
    Addition
      emptyPlayer
        { pName = "Rob Gronkowski",
          pTeams = all32TeamsPlusLegends,
          pPosition = Positions.te
        },
    Addition
      emptyPlayer
        { pName = "Willie Lanier",
          pTeams = withLegends [Team chiefs],
          pPosition = Positions.mlb
        },
    Addition
      emptyPlayer
        { pName = "Jay Hilgenberg",
          pTeams =
            withLegends
              [ Team bears,
                Team browns,
                Team saints
              ],
          pPosition = Positions.c
        },
    Addition
      emptyPlayer
        { pName = "Fred Biletnikoff",
          pTeams = withLegends [Team raiders],
          pPosition = Positions.wr
        },
    Replacement
      "Steve Atwater"
      emptyPlayer
        { pName = "Ed Reed",
          pTeams =
            withLegends
              [ Team ravens,
                Team jets,
                Team texans
              ]
        },
    Replacement
      "Rob Gronkowski"
      emptyPlayer
        { pName = "Rob Gronkowski",
          pTeams = comboOfTeams [all32TeamsPlusLegends, all32TeamsPlusLegends]
        }
  ]
