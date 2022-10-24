-- | Module: Functions.Display
module Functions.Display where

import Type
import Data.List

ppTeamOrMultiple :: TeamOrMultiple -> String
ppTeamOrMultiple (Team t) = t
ppTeamOrMultiple (MultipleTeam t i) = t ++ "x" ++ show i
ppTeamOrMultiple (Teams ts) = intercalate "/" $ map show ts

ppVariation :: Variation -> String
ppVariation = intercalate "\n" . map (\(p, t) -> p ++ ": " ++ ppTeamOrMultiple t)

ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation