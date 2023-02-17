{-# LANGUAGE FlexibleInstances #-}

module Types.Printable where

foo :: FooType a => String -> a
foo = bar  ""

class FooType a where
    bar :: String -> String -> a

instance FooType String where
    bar s acc = reverse s

instance {-# OVERLAPPING #-} (FooType r) => FooType (String -> r) where
    bar acc "" _ = bar "" acc
    bar acc ('%':'s':ss) x = bar (reverse x ++ acc) ss
    bar acc (s:ss) x = bar (s:acc) ss x

instance {-# OVERLAPPING #-} (Show x, FooType r) => FooType (x -> r) where
    bar acc ('%':'s':ss) x = bar (reverse (show x) ++ acc) ss

fooTest :: IO ()
fooTest = putStrLn $ foo "Test %s" "testington"