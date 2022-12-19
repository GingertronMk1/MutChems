-- |
-- Module: Data.Squad
module Data.Squad where

import           Data.Positions
import           Data.Teams
import           Types.ProspectiveChange
import           Types.TeamOrMultiple

-- | Base squad.
baseSquad :: InitialLineupObject
baseSquad = [
    PositionGroup {
      positionGroup = qb,
      players = [
        emptyPlayer {
          name = "Ryan Fitzpatrick",
          teams = [
            Team bengals,
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
        emptyPlayer {
          name = "Tim Tebow",
          teams = [
            Team legends,
            Team broncos,
            Team jets
          ]
        }
      ]},
    PositionGroup {
      positionGroup = hb,
      players = [
        emptyPlayer {name = "CJ2K", teams = [Team legends, Team titans, Team jets, Team cardinals]},
        emptyPlayer {name = "Rod Woodson (HB)", teams = [Team legends, Team raiders, Team niners, Team steelers, Team ravens]},
        emptyPlayer {name = "Travis Etienne Jr", teams = [Team jaguars]}
      ]},
    PositionGroup {
      positionGroup = fb,
      players = [
        emptyPlayer {name = "Nightmare", teams = [Team legends, Team chiefs]},
        emptyPlayer {name = "Jim Taylor", teams = [Team legends, Team packers, Team saints]}
      ]},
    PositionGroup {
      positionGroup = te,
      players = [
        emptyPlayer {name = "Dave Casper", teams = all32TeamsPlusLegends},
        emptyPlayer {name = "David Njoku", teams = [Team browns]},
        emptyPlayer {name = "Hayden Hurst", teams = [Team ravens, Team falcons, Team bengals]}
      ]},
    PositionGroup {
      positionGroup = wr,
      players = [
        emptyPlayer {name = "Calvin Johnson", teams = [Team lions, Team legends]},
        emptyPlayer {name = "Randy Moss", teams = [Team legends, Team titans, Team raiders, Team niners, Team vikings, Team patriots]},
        emptyPlayer {name = "Devin Hester", teams = [Team legends, Team ravens, Team falcons, Team seahawks, Team bears]},
        emptyPlayer {name = "Deion Sanders (WR)", teams = [ Team legends, Team ravens, Team niners, Team falcons, Team commanders, Team cowboys]},
        emptyPlayer {name = "Charlie Joiner", teams = [Team legends, Team bengals, Team chargers, Team titans]}
      ]},
    PositionGroup {
      positionGroup = lt,
      players = [
        emptyPlayer {name = "Jonathan Ogden", teams = [Team legends, Team ravens]},
        emptyPlayer {name = "Anthony Munoz", teams = [Team legends, Team bengals]}
      ]},
    PositionGroup {
      positionGroup = lg,
      players = [
        emptyPlayer {name = "Quenton Nelson", teams = [Team colts]},
        emptyPlayer {name = "Alan Faneca", teams = [Team legends, Team steelers, Team jets, Team cardinals]}
      ]},
    PositionGroup {
      positionGroup = c,
      players = [
        emptyPlayer {name = "Russ Grimm", teams = [Team legends, Team commanders]},
        emptyPlayer {name = "Creed Humphrey", teams = [Team chiefs]}
      ]},
    PositionGroup {
      positionGroup = rg,
      players = [
        emptyPlayer {name = "Larry Allen", teams = [Team legends, Team niners, Team cowboys]},
        emptyPlayer {name = "Will Shields", teams = [Team legends, Team chiefs]}
      ]},
    PositionGroup {
      positionGroup = rt,
      players = [
        emptyPlayer {name = "Willie Anderson", teams = [Team legends, Team bengals, Team ravens]},
        emptyPlayer {name = "Gary Zimmerman", teams = [Team legends, Team broncos, Team vikings]}
      ]},
    PositionGroup {
      positionGroup = mlb,
      players = [
        emptyPlayer {name = "Devin Bush", teams = [Team steelers]},
        emptyPlayer {name = "Derrick Thompson", teams = [Team legends, Team chiefs, Team raiders]},
        emptyPlayer {name = "Bobby Wagner", teams = [Team seahawks, Team rams]},
        emptyPlayer {name = "Kiko Alonso", teams = [Team legends, Team bills, Team dolphins, Team eagles, Team saints]}
      ]},
    PositionGroup {
      positionGroup = rolb,
      players = [
        emptyPlayer {name = "Khalil Mack", teams = [Team raiders, Team bears, Team chargers]},
        emptyPlayer {name = "Jonathan Casillas", teams = [Team legends, Team buccaneers, Team giants, Team patriots, Team saints]}
      ]},
    PositionGroup {
      positionGroup = lolb,
      players = [
        emptyPlayer {name = "Ted Hendricks", teams = [Team colts, Team packers, Team raiders, Team legends]},
        emptyPlayer {name = "Carl Banks", teams = [Team legends, Team browns, Team commanders, Team giants]}
      ]},
    PositionGroup {
      positionGroup = dt,
      players = [
        emptyPlayer {name = "Richard Seymour", teams = [Team legends, Team patriots, Team raiders]},
        emptyPlayer {name = "Sam Adams", teams =  all32TeamsPlusLegends},
        emptyPlayer {name = "Tony Siragusa", teams = [Team legends, Team ravens, Team colts]},
        emptyPlayer {name = "Merlin Olsen", teams = [Team legends, Team rams]}
      ]},
    PositionGroup {
      positionGroup = le,
      players = [
        emptyPlayer {name = "Jevon Kearse", teams = [Team legends, Team titans, Team eagles]},
        emptyPlayer {name = "George Karlaftis", teams = [Team broncos, Team buccaneers, Team cardinals, Team chiefs, Team giants, Team jaguars, Team niners, Team packers, Team saints, Team seahawks, Team vikings]}
      ]},
    PositionGroup {
      positionGroup = re,
      players = [
        emptyPlayer {name = "Cameron Wake", teams = [Team legends, Team titans, Team dolphins]},
        emptyPlayer {name = "Bruce Smith", teams = [Team legends, Team bills, Team commanders]}
      ]},
    PositionGroup {
      positionGroup = ss,
      players = [
        emptyPlayer {name = "John Lynch", teams = [Team legends, Team buccaneers, Team broncos]},
        emptyPlayer {name = "Nolan Cromwell", teams = [Team legends, Team rams]}
      ]},
    PositionGroup {
      positionGroup = fs,
      players = [
        emptyPlayer {name = "Sean Taylor", teams = [Team legends, Team commanders]},
        emptyPlayer {name = "Rod Woodson", teams = [Team legends, Team niners, Team steelers, Team raiders, Team ravens]}
      ]},
    PositionGroup {
      positionGroup = cb,
      players = [
        emptyPlayer {name = "Deion Sanders", teams = all32TeamsPlusLegends},
        emptyPlayer {name = "Benjamin St-Juste", teams = [Team commanders]},
        emptyPlayer {name = "Samari Rolle", teams = [Team legends, Team ravens, MultipleTeam titans 2]},
        emptyPlayer {name = "Adoree Jackson", teams = [Team giants, Team titans]},
        emptyPlayer {name = "Champ Bailey", teams = [Team commanders, Team broncos, Team legends]}
      ]},
    PositionGroup {
      positionGroup = k,
      players = [
        emptyPlayer {name = "Adam Vinatieri", teams = [Team legends, Team colts, Team patriots]}
      ]},
   PositionGroup {
      positionGroup = p,
      players = [
        emptyPlayer {name = "Ray Guy", teams = [Team legends, Team raiders]}
      ]
    }
  ]

-- | Team affinity strategy card.
strategy :: [TeamOrMultiple]
strategy = []

-- | Players I'm looking into.
prospectiveAdditions :: [ProspectiveChange]
prospectiveAdditions = [
    -- Zero chill forge
    Replacement "Hayden Hurst" (emptyPlayer {name = "Donald Parham", teams = [Team chargers]}),
    -- Campus Heroes to round out the legends
    Replacement "Bobby Wagner" (emptyPlayer {name = "Junior Seau", teams = [Team legends, Team chargers, Team dolphins, Team patriots]})
  ]

