-- |
-- Module: Data.Squad
module Data.Squad where

import qualified Data.Teams as T
import           Type

-- | Base squad.
baseSquad :: Lineup
baseSquad =
  [ -- qb
    ("Josh Allen", [Team T.bills]),
    ("Justin Fields", [Team T.bears]),
    -- hb
    ("Eric Dickerson", [Team T.legends, Team T.colts, Team T.rams, Team T.raiders, Team T.falcons]),
    ("Ezekiel Elliott", [Team T.cowboys]),
    ("Cordarrelle Patterson", [Team T.bears, Team T.raiders, Team T.patriots, Team T.vikings, Team T.falcons]),
    -- fb
    ("Patrick Ricard", [Team T.ravens]),
    ("Reggie Gilliam", [Team T.bills]),
    -- te
    ("Dave Casper", [Team T.all32Teams]),
    ("Evan Engram", [Team T.giants, Team T.jaguars]),
    ("Hayden Hurst", [Team T.ravens, Team T.falcons, Team T.bengals]),
    -- wr
    ("Devin Hester", [Team T.legends, Team T.ravens, Team T.bears, Team T.falcons, Team T.seahawks]),
    ("Tyreek Hill", [Team T.chiefs, Team T.dolphins]),
    ("Plaxico Burress", [Team T.legends, Team T.giants, Team T.jets, Team T.steelers]),
    ("Breshad Perriman", [Team T.bears, Team T.browns, Team T.buccaneers, Team T.jets, Team T.ravens]),
    ("Chase Claypool", [Team T.steelers, Team T.bears]),
    -- lt
    ("Garrett Bolles", [Team T.broncos]),
    ("Orlando Brown", [Team T.chiefs, Team T.ravens]),
    -- lg
    ("Quenton Nelson", [Team T.colts]),
    ("Andrew Norwell", [Team T.commanders, Team T.jaguars, Team T.panthers]),
    -- c
    ("Creed Humphrey", [Team T.chiefs]),
    ("Frank Ragnow", [Team T.lions]),
    -- rg
    ("Kevin Zeitler", [Team T.bengals, Team T.browns, Team T.giants, Team T.ravens]),
    ("Mark Glowinski", [Team T.giants, Team T.colts, Team T.seahawks]),
    -- rt
    ("Tristan Wirfs", [Team T.buccaneers]),
    ("Tom Compton", [Team T.niners, Team T.bears, Team T.broncos, Team T.commanders, Team T.falcons, Team T.jets, Team T.vikings]),
    -- mlb
    ("Devin White", [Team T.buccaneers]),
    ("Bobby Wagner", [Team T.seahawks, Team T.rams]),
    ("Roquan Smith", [Team T.bears, Team T.ravens]),
    ("Tremaine Edmunds", [Team T.bills]),
    -- rolb
    ("Khalil Mack", [Team T.raiders, Team T.bears, Team T.chargers]),
    ("Jonathan Casillas", [Team T.legends, Team T.buccaneers, Team T.giants, Team T.patriots, Team T.saints]),
    -- lolb
    ("Ted Hendricks", [Team T.colts, Team T.packers, Team T.raiders, Team T.legends]),
    -- dt
    ("Richard Seymour", [Team T.legends, Team T.patriots, Team T.raiders]),
    ("Sam Adams", [Team T.all32Teams]),
    ("Tony Siragusa", [Team T.ravens, Team T.colts]),
    ("Deforest Buckner", [Team T.niners, Team T.colts]),
    -- le
    ("Julius Peppers", [Team T.legends, Team T.panthers, Team T.packers, Team T.bears]),
    ("George Karlaftis", [Team T.chiefs, Team T.vikings, Team T.saints, Team T.giants, Team T.packers, Team T.broncos, Team T.jaguars, Team T.niners, Team T.cardinals, Team T.seahawks, Team T.buccaneers]),
    -- re
    ("Aaron Donald", [Team T.rams]),
    ("Aidan Hutchinson", [Team T.lions]),
    -- ss
    ("Isaiah Pola-Mao", [Team T.raiders]),
    ("Harrison Smith", [Team T.vikings]),
    -- fs
    ("Malik Hooker", [Team T.colts, Team T.cowboys]),
    ("Trevon Moehrig", [Team T.raiders]),
    -- cb
    ("Deion Sanders", [Team T.all32Teams]),
    ("Champ Bailey", [Team T.commanders, Team T.broncos, Team T.legends]),
    ("Shaquill Griffin", [Team T.seahawks, Team T.jaguars]),
    ("Rasul Douglas", [Team T.packers, Team T.eagles, Team T.panthers]),
    ("Sauce Gardner", [Team T.jets]),
    -- k
    ("Graham Gano", [Team T.giants, Team T.panthers, Team T.commanders]),
    -- p
    ("Johnny Hekker", [Team T.rams, Team T.panthers])
  ]

-- | Team affinity strategy card.
strategy :: PlayerTeams
strategy = ("None", [])

-- | Players I'm looking into.
prospectiveAdditions :: [ProspectiveAddition]
prospectiveAdditions =
  [
    Replacement "Trevon Moehrig" ("Eddie Jackson", [Team T.bears]),
    Replacement "Sauce Gardner" ("Jaycee Horn", [Team T.panthers]),
    Replacement "Deforest Buckner" ("DJ Reader", [Team T.texans, Team T.bengals]),
    Replacement "Rasul Douglas" ("Benjamin St Juste", [Team T.commanders]),
    Replacement "Tremaine Edmunds" ("Tremaine Edmunds", [Team T.bills]),
    Replacement "Justin Fields" ("Ryan Fitzpatrick", [Team T.legends, Team T.bengals, Team T.bills, Team T.buccaneers, Team T.commanders, Team T.dolphins, Team T.jets, Team T.rams, Team T.texans, Team T.titans])
  ]
