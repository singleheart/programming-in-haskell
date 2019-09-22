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

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where
    g x y z = (x, z)

instance Monad Parser
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
                                                      where
  p >>= f =
    P
      (\inp ->
         case parse p inp of
           [] -> []
           [(v, out)] -> parse (f v) out)

three' :: Parser (Char, Char)
three' = do
  x <- item
  item
  z <- item
  return (x, z)

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
