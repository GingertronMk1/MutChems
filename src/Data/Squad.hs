-- |
-- Module: Data.Squad
module Data.Squad where

import qualified Data.Teams as T
import Type


-- | Base squad
baseSquad :: Lineup
baseSquad =
  [ -- qb
    ("Justin Fields", [Team T.bears]),
    ("Tim Tebow", [Team T.broncos, Team T.jets, Team T.legends]),
    -- hb
    ("E. James Edge", [Team T.legends, Team T.seahawks, Team T.colts, Team T.cardinals]),
    ("Ezekiel Elliott", [Team T.cowboys]),
    ("Cordarrelle Patterson", [Team T.bears, Team T.raiders, Team T.patriots, Team T.vikings, Team T.falcons]),
    -- fb
    ("C.Okoye Nightmare", [Team T.chiefs, Team T.legends]),
    -- te
    ("Dave Casper", [Team T.all32Teams]),
    ("Evan Engram", [Team T.giants, Team T.jaguars]),
    -- wr
    ("Tyreek Hill", [Team T.chiefs, Team T.dolphins]),
    ("Deebo Samuel", [Team T.niners]),
    ("Chase Claypool", [Team T.steelers]),
    ("DJ Moore", [Team T.panthers]),
    -- lt
    ("Garrett Bolles", [Team T.broncos]),
    ("Orlando Brown", [Team T.chiefs, Team T.ravens]),
    -- lg
    ("Quenton Nelson", [Team T.colts]),
    ("Andrew Norwell", [Team T.commanders, Team T.jaguars, Team T.panthers]),
    -- c
    ("Frank Ragnow", [Team T.lions]),
    -- rg
    ("Mark Glowinski", [Team T.giants, Team T.colts, Team T.seahawks]),
    -- rt
    ("La'el Collins", [Team T.cowboys, Team T.bengals]),
    -- mlb
    ("Devin White", [Team T.buccaneers]),
    ("Bobby Wagner", [Team T.seahawks, Team T.rams]),
    ("Roquan Smith", [Team T.bears]),
    ("Tremaine Edmunds", [Team T.bills]),
    -- rolb
    ("Jonathan Casillas", [Team T.legends, Team T.buccaneers, Team T.giants, Team T.patriots, Team T.saints]),
    -- lolb
    ("Ted Hendricks", [Team T.colts, Team T.packers, Team T.raiders, Team T.legends]),
    -- dt
    ("Sam Adams", [Team T.all32Teams]),
    ("Deforest Buckner", [Team T.niners, Team T.colts]),
    -- le
    ("Julius Peppers", [Team T.legends, Team T.panthers, Team T.packers, Team T.bears]),
    -- re
    ("Aidan Hutchinson", [Team T.lions]),
    -- ss
    ("Isaiah Pola-Mao", [Team T.raiders]),
    ("Harrison Smith", [Team T.vikings]),
    -- fs
    ("Malik Hooker", [Team T.colts, Team T.cowboys]),
    ("Trevon Moehrig", [Team T.raiders]),
    -- cb
    ("Shaquill Griffin", [Team T.seahawks, Team T.jaguars]),
    ("Rasul Douglas", [Team T.packers, Team T.eagles, Team T.panthers]),
    ("Stephon Gilmore", [Team T.colts, Team T.bills, Team T.panthers, Team T.patriots]),
    ("Sauce Gardner", [Team T.jets]),
    -- k
    ("Zane Gonzalez", [Team T.browns, Team T.cardinals, Team T.panthers]),
    -- p
    ("Johnny Hekker", [Team T.rams, Team T.panthers])
  ]

-- | Team affinity strategy card
strategy :: PlayerTeams
strategy = ("None", [])

-- | Players I'm looking into
prospectiveAdditions :: Lineup
prospectiveAdditions =
  [
  ]