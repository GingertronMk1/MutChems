{-# LANGUAGE FlexibleInstances #-}

module Types.Printable where

printf :: Printable a => String -> a
printf = printf' ""

class Printable a where
  printf' :: String -> String -> a

instance Printable String where
  printf' acc s = reverse acc ++ s

instance {-# OVERLAPPING #-} (Printable r) => Printable (String -> r) where
  printf' acc "" _ = printf' "" acc
  printf' acc ('%' : 's' : ss) x = printf' (reverse x ++ acc) ss
  printf' acc (s : ss) x = printf' (s : acc) ss x

instance {-# OVERLAPPING #-} (Show x, Printable r) => Printable (x -> r) where
  printf' acc "" _ = printf' "" acc
  printf' acc ('%' : 's' : ss) x = printf' (reverse (show x) ++ acc) ss
  printf' acc (s : ss) x = printf' (s : acc) ss x
