data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)

-- this implementation is more efficient than guard because it uses less comparison for worse cases
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) =
  case compare x y of
    LT -> occurs x l
    EQ -> True
    GT -> occurs x r
