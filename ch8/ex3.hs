data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node a b) = abs (size a - size b) <= 1

size :: Tree a -> Int
size (Leaf _) = 1
size (Node a b) = size a + size b
