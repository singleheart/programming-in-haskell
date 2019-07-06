data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (size l - size r) <= 1 && balanced l && balanced r

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = size l + size r
