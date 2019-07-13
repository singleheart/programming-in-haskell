data Op
  = Add
  | Sub
  | Mul
  | Div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True -- MODIFY HERE
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0 -- MODIFY HERE

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr
  = Val Int
  | App Op Expr Expr

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

choices :: [a] -> [[a]]
choices = concat . map perms . subs

ops :: [Op]
ops = [Add, Sub, Mul, Div]

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

main :: IO ()
main = print (length [e | ns' <- choices [1, 3, 7, 10, 25, 50], e <- exprs ns', i <- eval e]) -- 10839369
