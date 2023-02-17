{-# LANGUAGE FlexibleInstances #-}

module Types.Printable where

myPrintF :: Printable a => String -> a
myPrintF = myPrintF' ""

class Printable a where
    myPrintF' :: String -> String -> a

instance Printable String where
    myPrintF' s _ = reverse s

instance {-# OVERLAPPING #-} (Printable r) => Printable (String -> r) where
    myPrintF' acc "" _ = myPrintF' "" acc
    myPrintF' acc ('%':'s':ss) x = myPrintF' (reverse x ++ acc) ss
    myPrintF' acc (s:ss) x = myPrintF' (s:acc) ss x

instance {-# OVERLAPPING #-} (Show x, Printable r) => Printable (x -> r) where
    myPrintF' acc ('%':'s':ss) x = myPrintF' (reverse (show x) ++ acc) ss
    myPrintF' acc (s:ss) x = myPrintF' (s:acc) ss x
    myPrintF' acc "" _ = myPrintF' acc ""

pfTest :: IO ()
pfTest = putStrLn $ myPrintF "Test %s" "testington"