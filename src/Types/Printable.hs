{-# LANGUAGE FlexibleInstances #-}

module Types.Printable where

foo :: FooType a => a
foo = bar "what"

class FooType a where
    bar :: String -> a

instance FooType String where
    bar = id

instance {-# OVERLAPPING #-} (FooType r) => FooType (String -> r) where
    bar s x = bar (s ++ x)

instance {-# OVERLAPPING #-} (Show x, FooType r) => FooType (x -> r) where
    bar s x = bar (s ++ show x)
