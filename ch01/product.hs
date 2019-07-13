module Product
  ( product'
  ) where

product' :: Num p => [p] -> p
product' [] = 1
product' (x:xs) = x * product' xs
