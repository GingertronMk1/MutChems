-- | Module: Data.Other
--
-- Just useful bits and bobs - should maybe be an env file? Idk
module Data.Other where

import Data.Char (isDigit)

-- | A key/value list to use with the `lookup` function to replace certain
-- characters with their HTML "non-breaking" versions
unBrokenCharacters :: [(Char, String)]
unBrokenCharacters =
  [ (' ', "&nbsp;"),
    ('-', "&#8209;")
  ]

-- | The threshold as a string
squadFilterThresholdString :: String
squadFilterThresholdString = "1 000 000"

-- | The maximum number of Variations per Lineup
squadFilterThreshold :: Int
squadFilterThreshold =
  read
    . filter isDigit
    $ squadFilterThresholdString
