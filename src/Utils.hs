module Utils where

clamp :: Ord a => a -> a -> a -> a
clamp lower upper x
  | x < lower = lower
  | x > upper = upper
  | otherwise = x
