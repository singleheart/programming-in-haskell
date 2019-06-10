concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

result = Main.concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]
