instance Foldable Maybe where
    -- fold :: Monoid a => Maybe a -> a
    fold Nothing = mempty
    fold (Just a) = a

    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap _ Nothing = mempty
    foldMap f (Just a) = f a

    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr _ _ Nothing = mempty
    foldr f v (Just a) = f a v

    -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
    foldl _ _ Nothing = mempty
    foldl f v (Just b) = f v b
