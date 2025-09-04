import Parsing
import Control.Applicative
import GHC.Internal.Read (paren)
import Text.XHtml (base)
import Data.Char
import CustomParsing
import Data.Sequence (Seq(Empty))
-- Ejercicio 2
-- Tenemos la gramática
-- expr -> term ('+' expr | '-' expr | {empty})
-- term -> factor ('*' term | '/' term | {empty})
-- factor -> '(' expr ')' | n
-- n -> d | dn
-- d -> '0' | '1' | ... | '9'
-- Construimos el parser para esta gramática
-- para empezar buscamos las asociaciones del modulo Parsing con cada producción de la gramática:
-- n se corresponde con natural (o nat tomando en cuenta espacios) por lo que  ya lo otenemos
-- factor no se encuentra, lo definimos

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> natural

-- term no se encuentra, lo definimos

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
            <|> do char '/'
                   t <- term
                   return (div f t)
                  <|> return f

-- expr no se encuentra, lo definimos
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t+e)
            <|> do symbol "-"
                   e <- expr
                   return (t-e)
                  <|> return t


-- Ejercicio 3
parenthesis :: Parser a -> Parser a
parenthesis p = do symbol "("
                   v <- parenthesis p
                   symbol ")"
                   return v
                  <|> do x <- p
                         return x


-- Ejercicio 4
-- Definimos la estructura del AST de expresiones aritméticas como
data Expr = Num Int | BinOp Op Expr Expr
       deriving (Eq, Show)
data Op = Add | Mul | Min | Div
       deriving (Eq, Show)

-- Refedinimos el parser del ejercicio 2 para que en lugar de evaluar una expresión genere un árbol de sintaxis abstracta
factor1 :: Parser Expr
factor1 = do symbol "("
             e <- expr1
             symbol ")"
             return e
            <|> do n <- natural
                   return (Num n)


term1 :: Parser Expr
term1 = do f <- factor1
           do symbol "*"
              t <- term1
              return (BinOp Mul f t)
            <|> do char '/'
                   t <- term1
                   return (BinOp Div f t)
                  <|> return f

expr1 :: Parser Expr
expr1 = do t <- term1
           do symbol "+"
              e <- expr1
              return (BinOp Add t e)
            <|> do symbol "-"
                   e <- expr1
                   return (BinOp Min t e)
                  <|> return t


-- Ejercicio 5

-- Podemos modelizar una subfamilia de los tipos de datos de Haskell 
data Basetype = DInt | DChar | DFloat 
              deriving (Eq, Show)
type Hasktype = [Basetype]

-- Al tipo Int -> Char -> Float podemos representarlo como [DInt, DChar, DFloat] :: Hasktype. Escribir un parser para esta subfamilia
-- Primero definimos la gramática
--
-- hasktype -> basetype ('->' hasktype | {empty}) 
-- basetype -> DInt | DChar | DFloat


basetype :: Parser Hasktype
basetype = do word "Int"
              return [DInt]
             <|> do word "Char"
                    return [DChar]
                   <|> do word "Float"
                          return [DFloat]
              
hasktype :: Parser Hasktype
hasktype = do ts <- basetype
              do symbol "->"
                 xs <- hasktype
                 return (ts ++ xs)
                <|> return ts

-- Ejercicio 6
-- Escribir un parser para listas heterogéneas de enteros y caracteres por extensión usando el formato de Haskell. 
-- Definimos el tipo de datos adecuado para representar estas listas
data HList = Cons HListElement HList | EmptyList
       deriving (Eq, Show)
data HListElement = Int Int | Char Char 
       deriving (Eq, Show)
-- Definimos la gramática para esta lista
-- hlist -> '[' hlistcontent ']' |
-- hlistcontent -> int (',' htlistcontent | e) | char (',' htlistcontent | e)

hlistcontent :: Parser HList
hlistcontent = do i <- int
                  do symbol ","
                     t <- hlistcontent
                     return (Cons (Int i) t)
                    <|> return (Cons (Int i) EmptyList)
                 <|> do char '\''
                        c <- alphanum
                        char '\''
                        do symbol ","
                           t <- hlistcontent
                           return (Cons (Char c) t)
                          <|> return (Cons (Char c) EmptyList)
                        
hlist :: Parser HList
hlist = do symbol "["
           t <- hlistcontent
           symbol "]"
           return t