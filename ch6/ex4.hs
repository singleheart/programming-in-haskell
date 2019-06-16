euclid :: Int -> Int -> Int
euclid 1 _ = 1
euclid _ 1 = 1
euclid a b
  | a == b = a
  | a > b = euclid (a - b) b
  | otherwise = euclid (b - a) a
