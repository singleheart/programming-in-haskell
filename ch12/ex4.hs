newtype ZipList a =
  Z [a]
  deriving (Show)

instance Functor ZipList
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
                                                  where
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList
    -- pure :: a -> ZipList a
                              where
  pure x = Z (repeat x)
    -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]
