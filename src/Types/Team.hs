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

thresholds :: [(Team -> Bool, Int)]
thresholds = [
    ((== Legends), 40),
    ((/= Legends), 50),
    ((== ULOline), 8),
    ((/= Legends), 40)
  ]

overAThreshold ::
  Team ->
  Team ->
  Int ->
  Int ->
  Bool
overAThreshold = overAThreshold' thresholds

overAThreshold' ::
  [(Team -> Bool, Int)] ->
  Team ->
  Team ->
  Int ->
  Int ->
  Bool
overAThreshold' [] _ _ _ _ = False
overAThreshold' ((f, n):fns) t1 t2 i1 i2 =
  and [f t1, f t2, i1 >= n, i2 >= n] || overAThreshold' fns t1 t2 i1 i2