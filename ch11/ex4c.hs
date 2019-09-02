import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player
  = O
  | B
  | X
  deriving (Eq, Ord, Read, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- ex4b begin
empty :: Int -> Grid
empty size = replicate size (replicate size B)

-- ex4b end
full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g =
  if os <= xs
    then O
    else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

-- ex4b
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. length g - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- ex4b
putGrid :: Grid -> IO ()
putGrid grid = putStrLn . unlines . concat . interleave bar . map showRow $ grid
  where
    bar = [replicate ((length grid * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

-- ex4b
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < length g ^ 2 && concat g !! i == B

-- ex4b
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i
    then [chop (length g) (xs ++ [p] ++ ys)]
    else []
  where
    (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

tictactoe :: IO ()
tictactoe = run (empty size) O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a =
  Node a [Tree a]
  deriving (Show)

-- ex4c
findNode :: Eq a => a -> Tree a -> Tree a
findNode x t = findNode' x t []

findNode' :: Eq a => a -> Tree a -> [Tree a] -> Tree a
findNode' x (Node y []) [] = Node y []
findNode' x (Node y []) (sb1:sbs) =
  if x == y
    then Node y []
    else findNode' x sb1 sbs
findNode' x (Node y (t:ts)) [] =
  if x == y
    then Node y (t : ts)
    else findNode' x t ts
findNode' x (Node y (t:ts)) (sb1:sbs)
  | x == y = Node y (t : ts)
  | x == child = Node child ct
  | otherwise = Node sibling st
  where
    Node child ct = findNode' x t ts
    Node sibling st = findNode' x sb1 sbs

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

-- ex4b
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((length g ^ 2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

-- ex11.4c
bestmove :: Grid -> Player -> Tree Grid -> Grid
bestmove g p gt = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = findNode g gt
    Node (_, best) ts = minimax tree

-- ex11.4a,b
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  order <- getOrder "Choose your order (1 or 2): "
  size <- getNat "Board size: "
  if order == 1
    then play (empty size) O
    else play (empty size) X

-- ex11.4a
getOrder :: String -> IO Int
getOrder prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && (xs == "1" || xs == "2")
    then return (read xs)
    else getOrder prompt

-- ex11.4c
play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p (gametree g p)

-- ex11.4c
play' :: Grid -> Player -> Tree Grid -> IO ()
play' g p gt
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play' g p gt
      [g'] -> play g' (next p)
  | p == X = do
    putStr "Player X is thinking... "
    (play $! bestmove g p gt) (next p)
