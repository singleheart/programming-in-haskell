scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
