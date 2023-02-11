-- | Module: Data.Positions
--
-- List of positions
module Data.Positions where

import Types.Basic

-- | Quarterback
qb :: Position
qb = "Quarterback"

-- | Halfback
hb :: Position
hb = "Halfback"

-- | Fullback
fb :: Position
fb = "Fullback"

-- | Wide Receiver
wr :: Position
wr = "Wide Receiver"

-- | Tight End
te :: Position
te = "Tight End"

-- | Left Tackle
lt :: Position
lt = "Left Tackle"

-- | Left Guard
lg :: Position
lg = "Left Guard"

-- | Center
c :: Position
c = "Center"

-- | Right Guard
rg :: Position
rg = "Right Guard"

-- | Right Tackle
rt :: Position
rt = "Right Tackle"

-- | Free Safety
fs :: Position
fs = "Free Safety"

-- | Right Outside Linebacker
rolb :: Position
rolb = "Right Outside Linebacker"

-- | Middle Linebacker
mlb :: Position
mlb = "Middle Linebacker"

-- | Left Outside Linebacker
lolb :: Position
lolb = "Left Outside Linebacker"

-- | Strong Safety
ss :: Position
ss = "Strong Safety"

-- | Cornerback
cb :: Position
cb = "Cornerback"

-- | Right Defensive End
re :: Position
re = "Right Defensive End"

-- | Defensive Tackle
dt :: Position
dt = "Defensive Tackle"

-- | Left Defensive End
le :: Position
le = "Left Defensive End"

-- | Kicker
k :: Position
k = "Kicker"

-- | Punter
p :: Position
p = "Punter"

-- | Strategy Cards
strategyCard :: Position
strategyCard = "Strategy Card"

-- | The maximum number of players in any position, to use with `lookup`
numInPositions :: [(Position, Int)]
numInPositions =
  [ (qb, 2),
    (hb, 3),
    (fb, 2),
    (wr, 5),
    (te, 3),
    (lt, 2),
    (lg, 2),
    (c, 2),
    (rg, 2),
    (rt, 2),
    (fs, 2),
    (rolb, 2),
    (mlb, 4),
    (lolb, 2),
    (ss, 2),
    (cb, 5),
    (re, 2),
    (dt, 4),
    (le, 2),
    (k, 1),
    (p, 1),
    (strategyCard, 1)
  ]
