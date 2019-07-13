-- from ex. 4.1
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

balance :: [a] -> Tree a
balance [] = error "Empty list"
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
  where
    (l, r) = halve xs
