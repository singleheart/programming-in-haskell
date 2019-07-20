readInt :: IO Int
readInt = do
  x <- getLine
  return (read x :: Int)

readInts :: Int -> IO [Int]
readInts n = sequence (replicate n readInt)

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- readInt
  xs <- readInts n
  putStr "The total is "
  putStrLn (show (sum xs))
  return ()
