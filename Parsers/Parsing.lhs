Basado en:
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Modificado por Mauro Jaskelioff

> module Parsing where
>
> import Data.Char
> import Control.Monad
> import Control.Applicative hiding (many)

The monad of parsers
--------------------

> newtype Parser a              =  P (String -> [(a,String)])
>
> instance Functor Parser where
>    fmap = liftM
>
> instance Applicative Parser where
>    pure = return
>    (<*>) = ap
> 
> instance Monad Parser where
>    return v                   =  P (\inp -> [(v,inp)])
>    p >>= f                    =  P (\inp -> [ x | (v,out) <- parse p inp, x <- parse (f v) out])
>
> instance Alternative Parser where
>    empty = mzero
>    (<|>) = mplus
> 
> instance MonadPlus Parser where
>    mzero                      =  P (\_	 -> [])
>    p `mplus` q                =  P (\inp -> case parse p inp of
>                                                []        -> parse q inp
>                                                x         -> x)

Basic parsers
-------------

Siempre falla


> failure                       :: Parser a
> failure                       =  mzero
>

Consume el primer caracter

> item                          :: Parser Char
> item                          =  P (\inp -> case inp of
>                                                []     -> []
>                                                (x:xs) -> [(x,xs)])
> 

Aplica un parser 


> parse                         :: Parser a -> String -> [(a,String)]
> parse (P p) inp               =  p inp

Derived primitives
------------------

Consume el primer caracter si satisface p

> sat                           :: (Char -> Bool) -> Parser Char
> sat p                         =  do x <- item
>                                     if p x then return x else failure
> 

Consume el primer caracter si es un digito

> digit                         :: Parser Char
> digit                         =  sat isDigit
> 

Consume el primer caracter si es minuscula

> lower                         :: Parser Char
> lower                         =  sat isLower
> 

Consume el primer caracter si es mayuscula

> upper                         :: Parser Char
> upper                         =  sat isUpper
> 

Consume el primer caracter si es una letra

> letter                        :: Parser Char
> letter                        =  sat isAlpha
> 

Consume el primer caracter si es alfanumérico

> alphanum                      :: Parser Char
> alphanum                    =  sat isAlphaNum
> 

Consume el primer caracter si es igual a su argumento

> char                          :: Char -> Parser Char
> char x                        =  sat (== x)
> 

Consume los primeros caracteres que formen el argumento

> string                        :: String -> Parser String
> string []                     =  return []
> string (x:xs)                 =  do char x
>                                     string xs
>                                     return (x:xs)
> 

Se comporta 0 o más veces como el parser p

> many                          :: Parser a -> Parser [a]
> many p                        =  many1 p <|> return []
> 

Se comporta 1 o más veces como el parser p

> many1                         :: Parser a -> Parser [a]
> many1 p                       =  do v  <- p
>                                     vs <- many p
>                                     return (v:vs)
> 

Consume los primeros caracteres que formen un identificador

> ident                         :: Parser String
> ident                         =  do x  <- lower
>                                     xs <- many alphanum
>                                     return (x:xs)
> 

Consume los primeros caracteres que formen un natural

> nat                           :: Parser Int
> nat                           =  do xs <- many1 digit
>                                     return (read xs)
>

Consumen los primeros caracteres que formen un entero

> int                           :: Parser Int
> int                           =  do char '-'
>                                     n <- nat
>                                     return (-n)
>                                   <|> nat
> 

Consumen los primeros caracteres que formen espacios

> space                         :: Parser ()
> space                         =  do many (sat isSpace)
>                                     return ()
>	

Consume los primeros caracteres que comience con p e intercale cadenas en p y cadenas en sep

> sepBy                         :: Parser a -> Parser sep -> Parser [a]
> sepBy p sep                   =  sepBy1 p sep <|> return []
>
> sepBy1                        :: Parser a -> Parser sep -> Parser [a]
> sepBy1 p sep        		= do{ x <- p
>                         	    ; xs <- many (sep >> p)
>                         	    ; return (x:xs) }	
>

Consume los primeros caracteres que forme un ciclo de secuencia de p y sep

> endBy1                        :: Parser a -> Parser sep -> Parser [a]
> endBy1 p sep                  = many1 (do { x <- p; sep; return x })
>
> endBy                         :: Parser a -> Parser sep -> Parser [a]
> endBy p sep                   	= many (do{ x <- p; sep; return x })
>
>

Ignoring spacing
----------------

Consume la cadena que satisface p ignorando espacios antes y despues 

> token                         :: Parser a -> Parser a
> token p                       =  do space
>                                     v <- p
>                                     space
>                                     return v
> 

Consume la cadena que sea un identificador ignorando espacios antes y despues 

> identifier                    :: Parser String
> identifier                    =  token ident
> 

Consume la cadena que sea un natural ignorando espacios antes y despues 

> natural                       :: Parser Int
> natural                       =  token nat
> 

Consume la cadena que sea un entero ignorando espacios antes y despues 

> integer                       :: Parser Int
> integer                       =  token int
>

Elimina espacios antes y después de la prime cadena sin espacios 

> symbol                        :: String -> Parser String
> symbol xs                     =  token (string xs)
>