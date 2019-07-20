import System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

readLine :: IO String
readLine = readLine' ""

readLine' :: String -> IO String
readLine' xs = do
  x <- getCh
  case x of
    '\n' -> do
      putChar '\n'
      return xs
    '\DEL' ->
      if null xs
        then readLine' ""
        else do
          putStr "\b \b"
          readLine' (init xs)
    _ -> do
      putChar x
      readLine' (xs ++ [x])
