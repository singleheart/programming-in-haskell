merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
  | a < b = a : merge as (b:bs)
  | otherwise = b : merge (a:as) bs
