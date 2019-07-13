(||) :: Bool -> Bool -> Bool
a || b
  | a == b = a
  | otherwise = True
