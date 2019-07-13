removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne x (y:ys)
  | x == y = ys
  | otherwise = y : removeOne x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = x `elem` ys && isChoice xs (removeOne x ys)
