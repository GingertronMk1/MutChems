module Classes.Data where

class Data a where
  toData :: a -> String
  fromData :: String -> a
  writeToFile :: String -> a -> IO ()
  writeToFile fileName = writeFile fileName . toData
  readFromFile :: String -> IO a
  readFromFile fileName = do
    stringData <- readFile fileName
    return $ fromData stringData
