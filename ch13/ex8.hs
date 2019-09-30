import Control.Applicative
import Data.Char

newtype Parser a =
  P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item =
  P (\inp ->
       case inp of
         [] -> []
         (x:xs) -> [(x, xs)])

instance Functor Parser
  -- fmap :: (a -> b) -> Parser a -> Parser b
                                              where
  fmap g p =
    P
      (\inp ->
         case parse p inp of
           [] -> []
           [(v, out)] -> [(g v, out)])

instance Applicative Parser
  -- pure :: a -> Parser a
                           where
  pure v = P (\inp -> [(v, inp)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      (\inp ->
         case parse pg inp of
           [] -> []
           [(g, out)] -> parse (fmap g px) out)

instance Monad Parser
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
                                                      where
  p >>= f =
    P
      (\inp ->
         case parse p inp of
           [] -> []
           [(v, out)] -> parse (f v) out)

instance Alternative Parser
  -- empty :: Parser a
                       where
  empty = P (\inp -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      (\inp ->
         case parse p inp of
           [] -> parse q inp
           [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int =
  do char '-'
     n <- nat
     return (-n)
     <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- a. grammar
-- expr ::= (expr + | expr - | e) term

expr :: Parser Int
expr = do
  t <- term
  do e <- expr
     symbol "+"
     return (t + e)
     <|> do 
      symbol "-" 
      e <- expr
      return (t - e)
     <|> return t

term :: Parser Int
term = do
  f <- factor
  do symbol "*"
     t <- term
     return (f * t)
     <|> do
      symbol "/"
      t <- term
      return (f `div ` t)
     <|> return f

factor :: Parser Int
factor = do
  symbol "("
  e <- expr
  symbol ")"
  return e 
  <|> integer

eval :: String -> Int
eval xs =
  case parse expr xs of
    [(n, [])] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"
