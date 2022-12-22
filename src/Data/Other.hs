-- | Module: Data.Other
--
-- Just useful bits and bobs - should maybe be an env file? Idk
module Data.Other where

unBrokenCharacters :: [(Char, String)]
unBrokenCharacters = [
    (' ', "&nbsp;"),
    ('-', "&#8209;")
  ]

-- | The maximum number of Variations per Lineup
squadFilterThreshold :: Int
squadFilterThreshold = 10 * 1000000
