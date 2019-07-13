concat' :: [[a]] -> [a]
concat' [] = []
concat' [x] = x
concat' (x:xs) = x ++ concat' xs
