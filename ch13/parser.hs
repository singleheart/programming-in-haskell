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
