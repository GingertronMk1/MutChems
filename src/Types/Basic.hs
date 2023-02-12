-- | Module: Types.Basic
--
-- Alias types
module Types.Basic where

import Text.Printf

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
  deriving (Eq, Ord, Enum)

instance Show PositionData where
  show Quarterback = "Quarterback"
  show Halfback = "Halfback"
  show Fullback = "Fullback"
  show WideReceiver = "Wide Receiver"
  show TightEnd = "Tight End"
  show LeftTackle = "Left Tackle"
  show LeftGuard = "Left Guard"
  show Center = "Center"
  show RightGuard = "Right Guard"
  show RightTackle = "Right Tackle"
  show FreeSafety = "Free Safety"
  show StrongSafety = "Strong Safety"
  show Cornerback = "Cornerback"
  show RightOutsideLinebacker = "Right Outside Linebacker"
  show MiddleLinebacker = "Middle Linebacker"
  show LeftOutsideLinebacker = "Left Outside Linebacker"
  show RightDefensiveEnd = "Right Defensive End"
  show DefensiveTackle = "Defensive Tackle"
  show LeftDefensiveEnd = "Left Defensive End"
  show Kicker = "Kicker"
  show Punter = "Punter"
  show StrategyCard = "Strategy Card"
  show NoPosition = ""

readToPositionData :: String -> PositionData
readToPositionData s = case filter (/= ' ') s of
  "Quarterback" -> Quarterback
  "Halfback" -> Halfback
  "Fullback" -> Fullback
  "WideReceiver" -> WideReceiver
  "TightEnd" -> TightEnd
  "LeftTackle" -> LeftTackle
  "LeftGuard" -> LeftGuard
  "Center" -> Center
  "RightGuard" -> RightGuard
  "RightTackle" -> RightTackle
  "FreeSafety" -> FreeSafety
  "StrongSafety" -> StrongSafety
  "Cornerback" -> Cornerback
  "RightOutsideLinebacker" -> RightOutsideLinebacker
  "MiddleLinebacker" -> MiddleLinebacker
  "LeftOutsideLinebacker" -> LeftOutsideLinebacker
  "RightDefensiveEnd" -> RightDefensiveEnd
  "DefensiveTackle" -> DefensiveTackle
  "LeftDefensiveEnd" -> LeftDefensiveEnd
  "Kicker" -> Kicker
  "Punter" -> Punter
  "StrategyCard" -> StrategyCard
  _ -> error $ printf "Could not read %s as PositionData" s
