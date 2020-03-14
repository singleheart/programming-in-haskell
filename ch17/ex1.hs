data Expr
  = Val Int
  | Add Expr Expr
  | Throw
  | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) =
  case eval x of
    Just n ->
      case eval y of
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
  | PUSH (Maybe Int) Code
  | ADD Code
  | IFELSE Code Code
  deriving (Show)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH (Just n) c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' Throw c = PUSH Nothing c
comp' (Catch x h) c = IFELSE (comp' x c) (comp' h c)

type Stack = [Maybe Int]

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (m:n:s) = exec c (((+) <$> n <*> m) : s)
exec (IFELSE x h) s =
  case tried of
    [Nothing] -> exec h s
    [Just _] -> tried
    where tried = exec x s
