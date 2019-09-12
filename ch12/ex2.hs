instance Functor ((->) a)
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
                                              where
  fmap = (.)
