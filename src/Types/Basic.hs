-- | Module: Types.Basic
--
-- Alias types
module Types.Basic where

import Data.List
import Data.Maybe

-- | Team is shorthand for a String - it is just the name of a team.
type Team = String

-- | A string representation of a TeamOrMultiple
type EncodedTeamOrMultiple = String

-- | Player is shorthand for a String - it is just the name of a football player.
type PlayerName = String

-- | A position
type Position = String

data PositionData
  = Quarterback
  | Halfback
  | Fullback
  | WideReceiver
  | TightEnd
  | LeftTackle
  | LeftGuard
  | Center
  | RightGuard
  | RightTackle
  | FreeSafety
  | StrongSafety
  | Cornerback
  | RightOutsideLinebacker
  | MiddleLinebacker
  | LeftOutsideLinebacker
  | RightDefensiveEnd
  | DefensiveTackle
  | LeftDefensiveEnd
  | Kicker
  | Punter
  | StrategyCard
  | NoPosition
  deriving (Eq)

positionDatas :: [(PositionData, String)]
positionDatas =
  [ (Quarterback, "Quarterback"),
    (Halfback, "Halfback"),
    (Fullback, "Fullback"),
    (WideReceiver, "Wide Receiver"),
    (TightEnd, "Tight End"),
    (LeftTackle, "Left Tackle"),
    (LeftGuard, "Left Guard"),
    (Center, "Center"),
    (RightGuard, "Right Guard"),
    (RightTackle, "Right Tackle"),
    (FreeSafety, "Free Safety"),
    (StrongSafety, "Strong Safety"),
    (Cornerback, "Cornerback"),
    (RightOutsideLinebacker, "Right Outside Linebacker"),
    (MiddleLinebacker, "Middle Linebacker"),
    (LeftOutsideLinebacker, "Left Outside Linebacker"),
    (RightDefensiveEnd, "Right Defensive End"),
    (DefensiveTackle, "Defensive Tackle"),
    (LeftDefensiveEnd, "Left Defensive End"),
    (Kicker, "Kicker"),
    (Punter, "Punter"),
    (StrategyCard, "Strategy Card"),
    (NoPosition, "")
  ]

instance Enum PositionData where
  fromEnum p =
    fromMaybe
      (error $ "No position found for " ++ show p)
      (findIndex ((== p) . fst) positionDatas)
  toEnum = fst . (positionDatas !!)

instance Show PositionData where
  show p = fromMaybe (error "No show found for PositionData") (lookup p positionDatas)

instance Ord PositionData where
  compare pos1 pos2 = compare (fromEnum pos1) (fromEnum pos2)

readToPositionData :: String -> PositionData
readToPositionData s =
  fst $
    fromMaybe
      (error $ "No read instance for " ++ s)
      (find ((== s) . snd) positionDatas)
