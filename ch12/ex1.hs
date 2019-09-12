data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree
    -- fmap :: (a -> b) -> Tree a -> Tree b
                                            where
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)
