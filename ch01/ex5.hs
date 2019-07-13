qsort_uniq :: Ord a => [a] -> [a]
qsort_uniq [] = []
qsort_uniq (x:xs) = qsort_uniq smaller ++ [x] ++ qsort_uniq larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]
