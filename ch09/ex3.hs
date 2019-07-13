split :: [a] -> [([a], [a])]
split [] = []
split (x:xs) = ([], x:xs) : [(x : ls, rs) | (ls, rs) <- split xs]
