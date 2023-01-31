-- |
-- Module: Data.Calculated
--
-- There should be no type signatures of the format @a -> a@, these should all be
-- just variables more or less
module Data.Calculated where

import Data.Char
import Data.Other

-- | The default threshold as defined in Data.Other
squadFilterThreshold :: Int
squadFilterThreshold =
  read
    . filter isDigit
    $ squadFilterThresholdString
