import Control.Applicative
import Data.Char

newtype Parser a =
  P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

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

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x : xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

symbol :: String -> Parser String
symbol xs = token (string xs)

eol :: Char
eol = '\n'

comment :: Parser ()
comment = do
    symbol "--"
    many (sat (/= eol))
    return ()
