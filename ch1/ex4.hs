qsort_reverse :: Ord a => [a] -> [a]
qsort_reverse [] = []
qsort_reverse (x:xs) = qsort_reverse larger ++ [x] ++ qsort_reverse smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
