-- | Module: Classes.Data
-- A custom class to print things to data files.
-- Basically I don't need Aeson and I've got a thing about using language pragmas
module Classes.Data where

import Data.Functor

-- | The Data class, with methods to write a type to a string and read a file
-- from a string
class Data a where
  toData :: a -> String
  fromData :: String -> a
  writeToFile :: String -> a -> IO ()
  writeToFile fileName = writeFile fileName . toData
  readFromFile :: String -> IO a
  readFromFile fileName = readFile fileName <&> fromData
