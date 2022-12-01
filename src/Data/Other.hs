-- | Module: Data.Other
-- 
-- Just useful bits and bobs - should maybe be an env file? Idk
module Data.Other where

import Data.Teams
import Types.Basic

-- | The maximum number of Variations per Lineup
squadFilterThreshold :: Int
squadFilterThreshold = 10 * 1000000

preferences :: [Team]
preferences = [titans, legends, seahawks, eagles, raiders]