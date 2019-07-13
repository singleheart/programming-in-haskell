pyths :: (Num c, Eq c, Enum c) => c -> [(c, c, c)]
pyths n =
  [ (x, y, z)
  | x <- [1 .. n]
  , y <- [1 .. n]
  , z <- [1 .. n]
  , x ^ 2 + y ^ 2 == z ^ 2
  ]
