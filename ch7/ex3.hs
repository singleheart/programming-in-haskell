map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f =
  foldr
    (\x xs ->
       if f x
         then x : xs
         else xs)
    []
