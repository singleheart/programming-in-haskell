safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs