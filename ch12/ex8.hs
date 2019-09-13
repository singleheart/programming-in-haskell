type State = Int

newtype ST a =
  S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST
  -- fmap :: (a -> b) -> ST a -> ST b
                                      where
  fmap g st = do
    x <- st
    return (g x)

instance Applicative ST
  -- pure :: a -> ST a
                       where
  pure x = S (\s -> (x, s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    a <- stx
    return (f a)

instance Monad ST
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
                                            where
  st >>= f =
    S
      (\s ->
         let (x, s') = app st s
          in app (f x) s')

-- to test
-- try fst (app (alabel tree) 0)
-- result: Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
fresh :: ST Int
fresh = S (\n -> (n, n + 1))

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r
