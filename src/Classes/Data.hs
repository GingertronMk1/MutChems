module Classes.Data where

import Data.Functor

class Data a where
  toData :: a -> String
  fromData :: String -> a
  writeToFile :: String -> a -> IO ()
  writeToFile fileName = writeFile fileName . toData
  readFromFile :: String -> IO a
  readFromFile fileName = readFile fileName <&> fromData
