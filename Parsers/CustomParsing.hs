module CustomParsing where

import Data.Char
import Parsing
import Control.Applicative

-- Definimos un parser que acepta una palabra en minuscula o mayuscula 

word :: String -> Parser String
word [] = return []
word (x:xs) = do c <- char (toLower x) <|> char (toUpper x)
                 cs <- word xs
                 return (c:cs)

-- Definimos el evaluador 

eval :: Parser a -> String -> a
eval p xs = fst (head (parse p xs)) 