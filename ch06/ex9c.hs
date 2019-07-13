last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
