-- |
-- Module: Data
module Data where

import Type

-- | A useful shorthand for any player who can have all 32 teams
all32Teams :: Team
all32Teams = "all32"

-- | The offensive players
offense :: LiterateLineup
offense =
  [ ( "qb",
      [ ("Justin Fields", ["Bears"]),
        ("Tim Tebow", ["Broncos", "Jets", "Legends"])
      ]
    ),
    ( "hb",
      [ ("Eric Dickerson", ["Legends", "Rams", "Colts", "Falcons", "Raiders"]),
        ("Cordarrelle Patterson", ["Bears", "Falcons", "Raiders", "Patriots", "Vikings"]),
        ("Ezekiel Elliott", ["Cowboys"])
      ]
    ),
    ( "fb",
      [ ("Jim Taylor", ["Packers", "Saints", "Legends"])
      ]
    ),
    ( "te",
      [ ("Dave Casper", [all32Teams]),
        ("Evan Engram", ["Giants", "Jaguars"])
      ]
    ),
    ( "wr",
      [ ("Michael Crabtree", ["49ers", "Ravens", "Cardinals", "Raiders", "Legends"]),
        ("Deebo Samuel ", ["49ers"]),
        ("DJ Moore", ["Panthers"])
      ]
    ),
    ( "lt",
      [ ("Garrett Bolles", ["Broncos"]),
        ("Orlando Brown", ["Chiefs", "Ravens"])
      ]
    ),
    ( "lg",
      [ ("Andrew Norwell", ["Commanders", "Jaguars", "Panthers"])
      ]
    ),
    ( "c",
      [ ("Frank Ragnow", ["Lions"])
      ]
    ),
    ( "rg",
      [ ("Mark Glowinski", ["Giants", "Colts", "Seahawks"])
      ]
    ),
    ( "rt",
      [ ("La'el Collins", ["Cowboys", "Bengals"])
      ]
    )
  ]

-- | The defensive players
defense :: LiterateLineup
defense =
  [ ( "mlb",
      [ ("Kiko Alonso", ["Legends", "Bills", "Dolphins", "Eagles", "Saints"]),
        ("Tremaine Edmunds", ["Bills"]),
        ("Devin White", ["Buccaneers"])
      ]
    ),
    ( "rolb",
      [ ("Jonathan Casillas", ["Legends", "Buccaneers", "Giants", "Patriots", "Saints"])
      ]
    ),
    ( "lolb",
      [ ("Ted Hendricks", ["Colts", "Packers", "Raiders", "Legends"])
      ]
    ),
    ( "dt",
      [ ("Sam Adams", [all32Teams]),
        ("Deforest Buckner", ["49ers", "Colts"])
      ]
    ),
    ( "le",
      [ ("Chase Young", ["Commanders"])
      ]
    ),
    ( "re",
      [ ("Aidan Hutchinson", ["Lions"])
      ]
    ),
    ( "ss",
      [ ("Harrison Smith", ["Vikings"]),
        ("Grant Delpit", ["Browns"])
      ]
    ),
    ( "fs",
      [ ("Malik Hooker", ["Colts", "Cowboys"]),
        ("Trevon Moehrig", ["Raiders"])
      ]
    ),
    ( "cb",
      [ ("Stephon Gilmore", ["Colts", "Bills", "Panthers", "Patriots"]),
        ("Sauce Gardner", ["Jets"]),
        ("Denzel Ward", ["Browns"]),
        ("Rasul Douglas", ["Packers", "Eagles", "Panthers"])
      ]
    )
  ]

-- | The kicker and punter
specialTeams :: LiterateLineup
specialTeams =
  [ ( "k",
      [ ("Zane Gonzalez", ["Browns", "Cardinals", "Panthers"])
      ]
    ),
    ( "p",
      [ ("Johnny Hekker", ["Rams", "Panthers"])
      ]
    )
  ]

-- | Team affinity strategy card
strategy :: PlayerTeams
strategy = ("None", [])

-- | Players I'm looking into
prospectiveAdditions :: Lineup
prospectiveAdditions =
  [ ("Jaire Alexander", ["Packers"]),
    ("Quenton Nelson", ["Colts"]),
    ("Roquan Smith", ["Bears"])
  ]