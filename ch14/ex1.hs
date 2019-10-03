instance (Monoid a, Monoid b) => Monoid (a, b)
  -- mempty :: (a, b)
                      where
  mempty = (mempty, mempty)
  -- mappend :: (a, b) -> (a, b) -> (a, b)
  (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)
