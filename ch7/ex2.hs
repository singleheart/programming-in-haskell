-- a. all
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x y -> p x && y) True

all'' :: (a -> Bool) -> [a] -> Bool
all'' p [] = True
all'' p (x:xs) = p x && all'' p xs

-- b. any
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x y -> p x || y) False

any'' :: (a -> Bool) -> [a] -> Bool
any'' p [] = False
any'' p (x:xs) = p x || any'' p xs

-- c. takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p =
  foldr
    (\x xs ->
       if p x
         then x : xs
         else [])
    []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p [] = []
takeWhile'' p (x:xs)
  | p x = x : takeWhile'' p xs
  | otherwise = []

-- d. dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p =
  foldl
    (\xs x ->
       if p x
         then []
         else xs ++ [x])
    []

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' p [] = []
dropWhile'' p (x:xs)
  | p x = dropWhile'' p xs
  | otherwise = x : xs
