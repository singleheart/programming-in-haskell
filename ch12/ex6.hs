instance Monad ((->) r)
    -- return :: a -> (r -> a)
                               where
  return = const
    -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
    -- f r :: a
    -- g (f r) :: r -> b
    -- \r -> g (f r) :: r -> (r -> b)
  (>>=) f g r = g (f r) r
