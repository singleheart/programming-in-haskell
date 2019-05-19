sum' :: Num p => [p] -> p
sum' []     = 0
sum' (n:ns) = n + sum' ns
