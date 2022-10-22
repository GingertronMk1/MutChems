-- |
-- Module: Data
module Data where

import qualified Data.Teams as Teams
import Type


-- | The offensive players
offense :: Lineup
offense =
  [ -- qb
    ("Justin Fields", [Teams.bears]),
    ("Tim Tebow", [Teams.broncos, Teams.jets, Teams.legends]),
    -- hb
    ("Eric Dickerson", [Teams.legends, Teams.rams, Teams.colts, Teams.falcons, Teams.raiders]),
    ("Cordarrelle Patterson", [Teams.bears, Teams.falcons, Teams.raiders, Teams.patriots, Teams.vikings]),
    ("Ezekiel Elliott", [Teams.cowboys]),
    -- fb
    ("Jim Taylor", [Teams.packers, Teams.saints, Teams.legends]),
    -- te
    ("Dave Casper", [Teams.all32Teams]),
    ("Evan Engram", [Teams.giants, Teams.jaguars]),
    -- wr
    ("Tyreek Hill", [Teams.chiefs, Teams.dolphins]),
    ("Deebo Samuel", [Teams.niners]),
    ("Chase Claypool", [Teams.steelers]),
    ("DJ Moore", [Teams.panthers]),
    -- lt
    ("Garrett Bolles", [Teams.broncos]),
    ("Orlando Brown", [Teams.chiefs, Teams.ravens]),
    -- lg
    ("Quenton Nelson", [Teams.colts]),
    ("Andrew Norwell", [Teams.commanders, Teams.jaguars, Teams.panthers]),
    -- c
    ("Frank Ragnow", [Teams.lions]),
    -- rg
    ("Mark Glowinski", [Teams.giants, Teams.colts, Teams.seahawks]),
    -- rt
    ("La'el Collins", [Teams.cowboys, Teams.bengals])
  ]

-- | The defensive players
defense :: Lineup
defense =
  [ -- mlb
    ("Kiko Alonso", [Teams.legends, Teams.bills, Teams.dolphins, Teams.eagles, Teams.saints]),
    ("Tremaine Edmunds", [Teams.bills]),
    ("Devin White", [Teams.buccaneers]),
    ("Roquan Smith", [Teams.bears]),
    -- rolb
    ("Jonathan Casillas", [Teams.legends, Teams.buccaneers, Teams.giants, Teams.patriots, Teams.saints]),
    -- lolb
    ("Ted Hendricks", [Teams.colts, Teams.packers, Teams.raiders, Teams.legends]),
    -- dt
    ("Sam Adams", [Teams.all32Teams]),
    ("Deforest Buckner", [Teams.niners, Teams.colts]),
    -- le
    ("Chase Young", [Teams.commanders]),
    -- re
    ("Aidan Hutchinson", [Teams.lions]),
    -- ss
    ("Harrison Smith", [Teams.vikings]),
    ("Grant Delpit", [Teams.browns]),
    -- fs
    ("Malik Hooker", [Teams.colts, Teams.cowboys]),
    ("Trevon Moehrig", [Teams.raiders]),
    -- cb
    ("Stephon Gilmore", [Teams.colts, Teams.bills, Teams.panthers, Teams.patriots]),
    ("Sauce Gardner", [Teams.jets]),
    ("Denzel Ward", [Teams.browns]),
    ("Rasul Douglas", [Teams.packers, Teams.eagles, Teams.panthers])
  ]

-- | The kicker and punter
specialTeams :: Lineup
specialTeams =
  [ -- k
    ("Zane Gonzalez", [Teams.browns, Teams.cardinals, Teams.panthers]),
    -- p
    ("Johnny Hekker", [Teams.rams, Teams.panthers])
  ]

-- | Team affinity strategy card
strategy :: PlayerTeams
strategy = ("None", [])

-- | Players I'm looking into
prospectiveAdditions :: Lineup
prospectiveAdditions =
  [ ("Jaire Alexander", [Teams.packers])
  ]