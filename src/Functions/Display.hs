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
ppVariation (Variation vs) =
  intercalate "\n"
  . map ppVariation'2
  . sortBy ppVariation'1
  $ vs
  where ppVariation'1 (p1, t1) (p2, t2) =
          case compare t1 t2 of
            EQ -> compare p1 p2
            c -> c
        ppVariation'2 (p, t) = p ++ ": " ++ ppTeamOrMultiple t


-- | Prettily print many variations, separated cleanly
ppVariations :: [Variation] -> String
ppVariations = intercalate "\n---\n" . map ppVariation

-- | Prettily print some double-folded variations to a nice Markdown string
ppDoubleFoldedVariations :: [PlayerTeams] -> String
ppDoubleFoldedVariations dfvs = 
  let totalCols = length . snd . head $ dfvs
   in intercalate "\n" [
      "| Team | " ++ (intercalate " | " . map show $ [1..totalCols]) ++ " |",  -- Top row
      "|---|" ++ concat (replicate totalCols ":---:|"),  -- Separator
      intercalate "\n" . map ppDoubleFoldedVariations' $ dfvs
   ]

-- | Helper for the above - make a Markdown table row for a single PlayerTeam
ppDoubleFoldedVariations' :: PlayerTeams -> String
ppDoubleFoldedVariations' (p, ts) =
  "| **"
  ++ p
  ++ "** | "
  ++ (intercalate " | " . map ppTeamOrMultiple) ts
  ++ " |"