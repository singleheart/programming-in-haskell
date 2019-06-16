merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
  | a < b = a : merge as (b : bs)
  | otherwise = b : merge (a : as) bs

halve :: [a] -> ([a], [a])
halve xs = splitAt ((length xs) `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = halve xs
