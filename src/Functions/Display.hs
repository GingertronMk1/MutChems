-- | Module: Functions.Display
module Functions.Display where

import Type
import Data.List

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple NoTeam = "-"
ppTeamOrMultiple (Team t) = t
ppTeamOrMultiple (MultipleTeam t i) = t ++ "x" ++ show i
ppTeamOrMultiple (Teams ts) = intercalate "/" $ map show ts

-- | Prettily print a Variation
ppVariation :: Variation -> String
ppVariation = intercalate "\n" . map (\(p, t) -> p ++ ": " ++ ppTeamOrMultiple t)

-- | Prettily print many variations, separated cleanly
ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation

ppDoubleFoldedVariations :: [PlayerTeams] -> String
ppDoubleFoldedVariations dfvs = 
  let totalCols = length . snd . head $ dfvs
   in intercalate "\n" [
      "| Team | " ++ (intercalate " | " . map (\n -> show n) $ [1..totalCols]) ++ " |",  -- Top row
      "|---|" ++ concat (replicate totalCols ":---:|"),  -- Separator
      intercalate "\n" . map (ppDoubleFoldedVariations') $ dfvs
   ]

ppDoubleFoldedVariations' :: PlayerTeams -> String
ppDoubleFoldedVariations' (p, ts) =
  "| **"
  ++ p
  ++ "** |"
  ++ (intercalate " | " . map ppTeamOrMultiple) ts
  ++ " |"