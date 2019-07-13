dec2int :: [Int] -> Int
dec2int = foldl (\n d -> 10 * n + d) 0
