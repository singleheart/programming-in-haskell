safetail :: [a] -> [a]
safetail xs
  | null xs = []
  | otherwise = tail xs
