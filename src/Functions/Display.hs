-- | Module: Functions.Display
module Functions.Display where

import Type
import Data.List

ppVariation :: Variation -> String
ppVariation = intercalate "\n" . map (\(p, ts) -> p ++ ": " ++ show ts)

ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation