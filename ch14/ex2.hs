instance Monoid b => Monoid (a -> b)
  -- mempty :: (a -> b)
                        where
  mempty = const mempty
  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  fl `mappend` fr = \x -> fl x `mappend` fr x
