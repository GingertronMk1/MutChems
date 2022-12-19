-- |
-- Module: Data.Squad
module Data.Squad where

import           Data.Positions
import           Data.Teams
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | Base squad.
baseSquad :: InitialLineup
baseSquad =
  [ PositionGroup
      { pgPosition = qb,
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
      { pgPosition = hb,
        pgPlayers =
          [ emptyPlayer
              { pName = "CJ2K",
                pTeams = [Team legends, Team titans, Team jets, Team cardinals]
              },
            emptyPlayer
              { pName = "Rod Woodson (HB)",
                pTeams = [Team legends, Team raiders, Team niners, Team steelers, Team ravens]
              },
            emptyPlayer
              { pName = "Travis Etienne Jr",
                pTeams = [Team jaguars]
              }
          ]
      },
    PositionGroup
      { pgPosition = fb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Nightmare",
                pTeams = [Team legends, Team chiefs]
              },
            emptyPlayer
              { pName = "Jim Taylor",
                pTeams = [Team legends, Team packers, Team saints]
              }
          ]
      },
    PositionGroup
      { pgPosition = te,
        pgPlayers =
          [ emptyPlayer
              { pName = "Dave Casper",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "David Njoku",
                pTeams = [Team browns]
              },
            emptyPlayer
              { pName = "Hayden Hurst",
                pTeams = [Team ravens, Team falcons, Team bengals]
              }
          ]
      },
    PositionGroup
      { pgPosition = wr,
        pgPlayers =
          [ emptyPlayer
              { pName = "Calvin Johnson",
                pTeams = [Team lions, Team legends]
              },
            emptyPlayer
              { pName = "Randy Moss",
                pTeams = [Team legends, Team titans, Team raiders, Team niners, Team vikings, Team patriots]
              },
            emptyPlayer
              { pName = "Devin Hester",
                pTeams = [Team legends, Team ravens, Team falcons, Team seahawks, Team bears]
              },
            emptyPlayer
              { pName = "Deion Sanders (WR)",
                pTeams = [Team legends, Team ravens, Team niners, Team falcons, Team commanders, Team cowboys]
              },
            emptyPlayer
              { pName = "Charlie Joiner",
                pTeams = [Team legends, Team bengals, Team chargers, Team titans]
              }
          ]
      },
    PositionGroup
      { pgPosition = lt,
        pgPlayers =
          [ emptyPlayer
              { pName = "Jonathan Ogden",
                pTeams = [Team legends, Team ravens]
              },
            emptyPlayer
              { pName = "Anthony Munoz",
                pTeams = [Team legends, Team bengals]
              }
          ]
      },
    PositionGroup
      { pgPosition = lg,
        pgPlayers =
          [ emptyPlayer
              { pName = "Quenton Nelson",
                pTeams = [Team colts]
              },
            emptyPlayer
              { pName = "Alan Faneca",
                pTeams = [Team legends, Team steelers, Team jets, Team cardinals]
              }
          ]
      },
    PositionGroup
      { pgPosition = c,
        pgPlayers =
          [ emptyPlayer
              { pName = "Russ Grimm",
                pTeams = [Team legends, Team commanders]
              },
            emptyPlayer
              { pName = "Creed Humphrey",
                pTeams = [Team chiefs]
              }
          ]
      },
    PositionGroup
      { pgPosition = rg,
        pgPlayers =
          [ emptyPlayer
              { pName = "Larry Allen",
                pTeams = [Team legends, Team niners, Team cowboys]
              },
            emptyPlayer
              { pName = "Will Shields",
                pTeams = [Team legends, Team chiefs]
              }
          ]
      },
    PositionGroup
      { pgPosition = rt,
        pgPlayers =
          [ emptyPlayer
              { pName = "Willie Anderson",
                pTeams = [Team legends, Team bengals, Team ravens]
              },
            emptyPlayer
              { pName = "Gary Zimmerman",
                pTeams = [Team legends, Team broncos, Team vikings]
              }
          ]
      },
    PositionGroup
      { pgPosition = mlb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Devin Bush",
                pTeams = [Team steelers]
              },
            emptyPlayer
              { pName = "Derrick Thompson",
                pTeams = [Team legends, Team chiefs, Team raiders]
              },
            emptyPlayer
              { pName = "Junior Seau",
                pTeams = [Team legends, Team chargers, Team dolphins, Team patriots]
              },
            emptyPlayer
              { pName = "Kiko Alonso",
                pTeams = [Team legends, Team bills, Team dolphins, Team eagles, Team saints]
              }
          ]
      },
    PositionGroup
      { pgPosition = rolb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Khalil Mack",
                pTeams = [Team raiders, Team bears, Team chargers]
              },
            emptyPlayer
              { pName = "Jonathan Casillas",
                pTeams = [Team legends, Team buccaneers, Team giants, Team patriots, Team saints]
              }
          ]
      },
    PositionGroup
      { pgPosition = lolb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Ted Hendricks",
                pTeams = [Team colts, Team packers, Team raiders, Team legends]
              },
            emptyPlayer
              { pName = "Carl Banks",
                pTeams = [Team legends, Team browns, Team commanders, Team giants]
              }
          ]
      },
    PositionGroup
      { pgPosition = dt,
        pgPlayers =
          [ emptyPlayer
              { pName = "Richard Seymour",
                pTeams = [Team legends, Team patriots, Team raiders]
              },
            emptyPlayer
              { pName = "Sam Adams",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "Tony Siragusa",
                pTeams = [Team legends, Team ravens, Team colts]
              },
            emptyPlayer
              { pName = "Merlin Olsen",
                pTeams = [Team legends, Team rams]
              }
          ]
      },
    PositionGroup
      { pgPosition = le,
        pgPlayers =
          [ emptyPlayer
              { pName = "Jevon Kearse",
                pTeams = [Team legends, Team titans, Team eagles]
              },
            emptyPlayer
              { pName = "George Karlaftis",
                pTeams = [Team broncos, Team buccaneers, Team cardinals, Team chiefs, Team giants, Team jaguars, Team niners, Team packers, Team saints, Team seahawks, Team vikings]
              }
          ]
      },
    PositionGroup
      { pgPosition = re,
        pgPlayers =
          [ emptyPlayer
              { pName = "Cameron Wake",
                pTeams = [Team legends, Team titans, Team dolphins]
              },
            emptyPlayer
              { pName = "Bruce Smith",
                pTeams = [Team legends, Team bills, Team commanders]
              }
          ]
      },
    PositionGroup
      { pgPosition = ss,
        pgPlayers =
          [ emptyPlayer
              { pName = "John Lynch",
                pTeams = [Team legends, Team buccaneers, Team broncos]
              },
            emptyPlayer
              { pName = "Nolan Cromwell",
                pTeams = [Team legends, Team rams]
              }
          ]
      },
    PositionGroup
      { pgPosition = fs,
        pgPlayers =
          [ emptyPlayer
              { pName = "Sean Taylor",
                pTeams = [Team legends, Team commanders]
              },
            emptyPlayer
              { pName = "Rod Woodson",
                pTeams = [Team legends, Team niners, Team steelers, Team raiders, Team ravens]
              }
          ]
      },
    PositionGroup
      { pgPosition = cb,
        pgPlayers =
          [ emptyPlayer
              { pName = "Deion Sanders",
                pTeams = all32TeamsPlusLegends
              },
            emptyPlayer
              { pName = "Benjamin St-Juste",
                pTeams = [Team commanders]
              },
            emptyPlayer
              { pName = "Samari Rolle",
                pTeams = [Team legends, Team ravens, MultipleTeam titans 2]
              },
            emptyPlayer
              { pName = "Adoree Jackson",
                pTeams = [Team giants, Team titans]
              },
            emptyPlayer
              { pName = "Champ Bailey",
                pTeams = [Team commanders, Team broncos, Team legends]
              }
          ]
      },
    PositionGroup
      { pgPosition = k,
        pgPlayers =
          [ emptyPlayer
              { pName = "Adam Vinatieri",
                pTeams = [Team legends, Team colts, Team patriots]
              }
          ]
      },
    PositionGroup
      { pgPosition = p,
        pgPlayers =
          [ emptyPlayer
              { pName = "Ray Guy",
                pTeams = [Team legends, Team raiders]
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
  [ -- Zero chill forge
    Replacement
      "Hayden Hurst"
      emptyPlayer
        { pName = "Donald Parham",
          pTeams = [Team chargers]
        },
    Replacement
      "Adoree Jackson"
      emptyPlayer
        { pName = "Charles Tillman",
          pTeams = [Team legends, Team bears, Team panthers]
        },
    Replacement
      "Benjamin St-Juste"
      emptyPlayer
        { pName = "Ty Law",
          pTeams = [Team legends, Team broncos, Team chiefs, Team jets, Team patriots]
        }
  ]
