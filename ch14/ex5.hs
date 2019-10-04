filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\x -> if f x then x else mempty)
