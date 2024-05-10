concat' :: [[a]] -> [a]
concat' [[]] = []
concat' [[xs]] = xs
concat' (xs:xss) = xs ++ concat' xss
