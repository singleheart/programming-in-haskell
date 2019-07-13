and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)
