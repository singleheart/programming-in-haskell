readInt :: IO Int
readInt = do
  x <- getLine
  return (read x :: Int)

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do
  x <- readInt
  xs <- readInts (n - 1)
  return (x : xs)

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- readInt
  xs <- readInts n
  putStr "The total is "
  putStrLn (show (sum xs))
  return ()
