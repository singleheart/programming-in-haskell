data Expr
  = Val Int
  | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)
