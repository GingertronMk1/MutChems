{-|
Module: Data
-}
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
      [ ("Cordarrelle Patterson", ["Bears", "Falcons", "Raiders", "Patriots", "Vikings"]),
        ("Demarco Murray", ["Cowboys", "Eagles", "Titans", "Legends"])
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
      [
        ("Larry Fitzgerald", ["Cardinals", "Legends"]),
        ("Michael Crabtree", ["49ers", "Ravens", "Cardinals", "Raiders", "Legends"]),
        ("DJ Moore", ["Panthers"]),
        ("Brandin Cooks", ["Patriots", "Rams", "Saints", "Texans"])
      ]
    ),
    ( "lt",
      [ ("Orlando Brown", ["Chiefs", "Ravens"])
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
    ( "ss",
      [ ("Harrison Smith", ["Vikings"]),
        ("Grant Delpit", ["Browns"])
      ]
    ),
    ( "fs",
      [ ("Trevon Moehrig", ["Raiders"]),
        ("Marcus Williams", ["Saints", "Ravens"])
      ]
    ),
    ( "cb",
      [ ("Stephon Gilmore", ["Colts", "Bills", "Panthers", "Patriots"]),
        ("Sauce Gardner", ["Jets"]),
        ("Casey Hayward Jr", ["Raiders", "Packers", "Chargers", "Falcons"]),
        ("Sidney Jones IV", ["Seahawks", "Eagles", "Jaguars"]),
        ("Rasul Douglas", ["Packers", "Eagles", "Panthers"])
      ]
    ),
    ( "dt",
      [ ("Sam Adams", [all32Teams]),
        ("Deforest Buckner", ["49ers", "Colts"])
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

