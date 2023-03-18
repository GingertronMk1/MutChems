-- | Module: Types.Team
module Types.Team where

-- | An enumerable type containing every team in the league plus legends
data Team
  = Legends
  | Bears
  | Bengals
  | Bills
  | Broncos
  | Browns
  | Buccaneers
  | Cardinals
  | Chargers
  | Chiefs
  | Colts
  | Commanders
  | Cowboys
  | Dolphins
  | Eagles
  | Falcons
  | Giants
  | Jaguars
  | Jets
  | Lions
  | Niners
  | Packers
  | Panthers
  | Patriots
  | Raiders
  | Rams
  | Ravens
  | Saints
  | Seahawks
  | Steelers
  | Texans
  | Titans
  | Vikings
  | ULOline
  deriving (Eq, Ord, Show, Read)
