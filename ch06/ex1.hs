fac :: Int -> Int
fac 0 = 1
fac n
  | n > 0 = n * fac (n - 1)
  -- negative values will cause infinite loop thus are treated by ghci as exception
