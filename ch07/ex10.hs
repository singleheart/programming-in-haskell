altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f0 f1 (x:xs) = f0 x : altMap f1 f0 xs

luhnDouble :: Int -> Int
luhnDouble x =
  if x * 2 > 9
    then x * 2 - 9
    else x * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble $ reverse xs) `mod` 10 == 0
