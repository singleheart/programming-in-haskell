class Monoid' a where
  mempty :: a
  mappend :: a -> a -> a

instance (Monoid' a, Monoid' b) => Monoid' (a, b)
  -- mempty :: (a, b)
                      where
  mempty = (Main.mempty, Main.mempty)
  -- mappend :: (a, b) -> (a, b) -> (a, b)
  (x1, y1) `mappend` (x2, y2) = (x1 `Main.mappend` x2, y1 `Main.mappend` y2)
