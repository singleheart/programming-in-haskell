replicate :: (Num t, Enum t) => t -> a -> [a]
replicate n x = [x | _ <- [1..n]]