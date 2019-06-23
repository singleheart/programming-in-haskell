-- a. all
all :: (a -> Bool) -> [a] -> Bool
all f = and . map f

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (\x y -> f x && y) True

all'' :: (a -> Bool) -> [a] -> Bool
all'' f [] = True
all'' f (x:xs) = f x && all'' f xs

-- b. any
any :: (a -> Bool) -> [a] -> Bool
any f = or . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x y -> f x || y) False

any'' :: (a -> Bool) -> [a] -> Bool
any'' f [] = False
any'' f (x:xs) = f x || any'' f xs

-- c. takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f =
  foldr
    (\x y ->
       if f x
         then x : y
         else [])
    []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f [] = []
takeWhile'' f (x:xs) =
  if f x
    then x : takeWhile'' f xs
    else []

-- d. dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f =
  foldl
    (\x y ->
       if f y
         then []
         else x ++ [y])
    []

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' f [] = []
dropWhile'' f (x:xs) =
  if f x
    then dropWhile'' f xs
    else x : xs
