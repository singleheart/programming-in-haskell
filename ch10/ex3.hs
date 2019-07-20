type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow idx row | (idx, row) <- zip [1 ..] board]
