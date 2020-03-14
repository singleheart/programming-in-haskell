data Expr
  = Val Int
  | Add Expr Expr
  | Throw
  | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = 
  case eval x of 
    Just n -> case eval y of 
      Just m -> Just (n + m)
      Nothing -> Nothing
    Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) =
  case eval x of
    Just n -> Just n
    Nothing -> eval h

data Code
  = HALT
  | PUSH Int Code
  | ADD Code
  deriving (Show)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

type Stack = [Int]

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m:n:s) = exec c (n + m : s)
