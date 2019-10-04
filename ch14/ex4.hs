import Data.Foldable

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree
  -- fmap :: (a -> b) -> Tree a -> Tree b
                                          where
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree
    -- fold :: Monoid a => Tree a -> a
                                       where
  fold Leaf = mempty
  fold (Node l a r) = fold l `mappend` a `mappend` fold r
    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r
    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ v Leaf = v
  foldr f v (Node l a r) = foldr f (foldr f (f a v) r) l
    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf = v
  foldl f v (Node l b r) = foldl f (foldl f (f v b) l) r

instance Traversable Tree
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
                                                                       where
  traverse g Leaf = pure Leaf
  traverse g (Node l a r) = pure Node <*> traverse g l <*> g a <*> traverse g r
