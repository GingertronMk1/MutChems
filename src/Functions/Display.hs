-- | Module: Functions.Display
module Functions.Display where

import Type
import Data.List

-- | Pretty print a TeamOrMultiple - basically `show` but a bit nicer
ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple (Team t) = t
ppTeamOrMultiple (MultipleTeam t i) = t ++ "x" ++ show i
ppTeamOrMultiple (Teams ts) = intercalate "/" $ map show ts

-- | Prettily print a Variation
ppVariation :: Variation -> String
ppVariation = intercalate "\n" . map (\(p, t) -> p ++ ": " ++ ppTeamOrMultiple t)

-- | Prettily print many variations, separated cleanly
ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation