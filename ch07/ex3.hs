map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p =
  foldr
    (\x xs ->
       if p x
         then x : xs
         else xs)
    []
