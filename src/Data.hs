-- |
-- Module: Data
module Data where

import qualified Data.Teams as Teams
import Type


-- | Base squad
baseSquad :: Lineup
baseSquad =
  [ -- qb
    ("Justin Fields", [Team Teams.bears]),
    ("Tim Tebow", [Team Teams.broncos, Team Teams.jets, Team Teams.legends]),
    -- hb
    ("Eric Dickerson", [Team Teams.legends, Team Teams.rams, Team Teams.colts, Team Teams.falcons, Team Teams.raiders]),
    ("LaDainian Tomlinson", [Team Teams.chargers, Team Teams.jets, Team Teams.legends]),
    ("Ezekiel Elliott", [Team Teams.cowboys]),
    -- fb
    ("Jim Taylor", [Team Teams.packers, Team Teams.saints, Team Teams.legends]),
    -- te
    ("Dave Casper", [Team Teams.all32Teams]),
    ("Evan Engram", [Team Teams.giants, Team Teams.jaguars]),
    -- wr
    ("Tyreek Hill", [Team Teams.chiefs, Team Teams.dolphins]),
    ("Deebo Samuel", [Team Teams.niners]),
    ("Chase Claypool", [Team Teams.steelers]),
    ("DJ Moore", [Team Teams.panthers]),
    -- lt
    ("Garrett Bolles", [Team Teams.broncos]),
    ("Orlando Brown", [Team Teams.chiefs, Team Teams.ravens]),
    -- lg
    ("Quenton Nelson", [Team Teams.colts]),
    ("Andrew Norwell", [Team Teams.commanders, Team Teams.jaguars, Team Teams.panthers]),
    -- c
    ("Frank Ragnow", [Team Teams.lions]),
    -- rg
    ("Mark Glowinski", [Team Teams.giants, Team Teams.colts, Team Teams.seahawks]),
    -- rt
    ("La'el Collins", [Team Teams.cowboys, Team Teams.bengals]),
    -- mlb
    ("Kiko Alonso", [Team Teams.legends, Team Teams.bills, Team Teams.dolphins, Team Teams.eagles, Team Teams.saints]),
    ("Tremaine Edmunds", [Team Teams.bills]),
    ("Devin White", [Team Teams.buccaneers]),
    ("Roquan Smith", [Team Teams.bears]),
    -- rolb
    ("Jonathan Casillas", [Team Teams.legends, Team Teams.buccaneers, Team Teams.giants, Team Teams.patriots, Team Teams.saints]),
    -- lolb
    ("Ted Hendricks", [Team Teams.colts, Team Teams.packers, Team Teams.raiders, Team Teams.legends]),
    -- dt
    ("Sam Adams", [Team Teams.all32Teams]),
    ("Deforest Buckner", [Team Teams.niners, Team Teams.colts]),
    -- le
    ("Chase Young", [Team Teams.commanders]),
    -- re
    ("Aidan Hutchinson", [Team Teams.lions]),
    -- ss
    ("Harrison Smith", [Team Teams.vikings]),
    ("Grant Delpit", [Team Teams.browns]),
    -- fs
    ("Malik Hooker", [Team Teams.colts, Team Teams.cowboys]),
    ("Trevon Moehrig", [Team Teams.raiders]),
    -- cb
    ("Stephon Gilmore", [Team Teams.colts, Team Teams.bills, Team Teams.panthers, Team Teams.patriots]),
    ("Sauce Gardner", [Team Teams.jets]),
    ("Denzel Ward", [Team Teams.browns]),
    ("Rasul Douglas", [Team Teams.packers, Team Teams.eagles, Team Teams.panthers]),
    -- k
    ("Zane Gonzalez", [Team Teams.browns, Team Teams.cardinals, Team Teams.panthers]),
    -- p
    ("Johnny Hekker", [Team Teams.rams, Team Teams.panthers])
  ]

-- | Team affinity strategy card
strategy :: PlayerTeams
strategy = ("None", [])

-- | Players I'm looking into
prospectiveAdditions :: Lineup
prospectiveAdditions =
  [ ("Jaire Alexander", [Team Teams.packers])
  ]