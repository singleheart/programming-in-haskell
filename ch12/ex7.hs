data Expr a
  = Var a
  | Val Int
  | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap _ (Val x) = Val x
  fmap g (Var x) = Var (g x)
  fmap g (Add a b) = Add (fmap g a) (fmap g b)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var
  
  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Val x <*> _ = error "Cannot apply a non-function value"
  _ <*> Val x = error "Cannot apply a non-function value"
  Var f <*> x = fmap f x
  Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
  return = pure
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Val x >>= _ = Val x
  Var x >>= f = f x
  Add a b >>= f = Add (a >>= f) (b >>= f)

--examples
-- Var 5 >>= \x -> Var (x*3)
-- result: Var 15
--
-- Add (Var 2) (Var 3) >>= \x -> Var (x * 2)
-- result: Add (Var 4) (Var 6)
