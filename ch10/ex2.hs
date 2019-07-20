type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoardRow :: Int -> Board -> IO ()
putBoardRow _ [] = return ()
putBoardRow r (num:board) = do
  putRow r num
  putBoardRow (r + 1) board

putBoard :: Board -> IO ()
putBoard = putBoardRow 1
