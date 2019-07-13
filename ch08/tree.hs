data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r)
  | x == y = True
  | x < y = occurs x l
  | otherwise = occurs x r
