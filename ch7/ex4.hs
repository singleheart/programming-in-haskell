dec2int :: [Int] -> Int
dec2int = foldl (\n d -> n * 10 + d) 0
