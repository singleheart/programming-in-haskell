sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x:xs) = x + sum' xs
