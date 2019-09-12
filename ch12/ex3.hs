instance Applicative ((->) r)
  -- pure :: a -> (r -> a)
                           where
  pure = const
  -- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
  (<*>) f g x = f x (g x)
