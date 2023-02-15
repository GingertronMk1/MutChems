module Types.Position where

import Data.List
import Data.Maybe
import Types.Basic

-- | A position
data Position
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

positionDatas :: [(Position, EncodedPosition)]
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

instance Enum Position where
  fromEnum p =
    fromMaybe
      (error $ "No position found for " ++ show p)
      (findIndex ((== p) . fst) positionDatas)
  toEnum = fst . (positionDatas !!)

instance Show Position where
  show p =
    fromMaybe
      (error "No show found for Position")
      (lookup p positionDatas)

instance Ord Position where
  compare pos1 pos2 =
    compare
      (fromEnum pos1)
      (fromEnum pos2)

readToPositionData :: EncodedPosition -> Position
readToPositionData s =
  fst $
    fromMaybe
      (error $ "No read instance for " ++ s)
      (find ((== s) . snd) positionDatas)
