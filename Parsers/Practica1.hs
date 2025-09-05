import Parsing
import Control.Applicative
import GHC.Internal.Read (paren)
import Text.XHtml (base, ddef)
import Data.Char
import CustomParsing
import Data.Sequence (Seq(Empty))
import Data.Kind (Type)
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
-- basetype -> 'Int' | 'Char' | 'Float'


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

-- Ejercicio 7
-- Podemos modelizar otra subfamilia de los tipos de datos de Haskell, más expresiva que la del ejercicio 5, mediante el siguiente tipo de datos
data Hasktype1 = D1Int | D1Char | D1Float | Fun Hasktype1 Hasktype1      
              deriving (Show, Eq)
-- Por ejemplo el tipo Int -> Char -> Float será representado mediante el término Fun D1Int (Fun D1Char D1Float) mientras que el tipo (Int -> Char) -> Float, que no 
-- pertenece a la subfamilia del ejercicio 5, será representado por Fun (Fun D1Int D1Char) D1Float
-- Se va a escribir un parser para esta subfamilia de tipos de Haskell
-- Primero, escribiremos la gramática correspondiente
--
-- hasktype1 -> hasktypeAtom1 ('->' hasktype1 | {empty})
-- hasktypeAtom1 -> 'Int' | 'Char' | 'Float' | '(' hasktype1 ')'
-- Luego

hasktype1 :: Parser Hasktype1
hasktype1 = do t1 <- hasktypeAtom1
               do symbol "->"
                  t2 <- hasktype1 
                  return (Fun t1 t2)
                 <|> return t1


hasktypeAtom1 :: Parser Hasktype1
hasktypeAtom1 = do word "Int"
                   return D1Int
                  <|> do word "Char"
                         return D1Char
                        <|> do word "Float"
                               return D1Float
                              <|> do symbol "("
                                     t <- hasktype1
                                     symbol ")"
                                     return t

-- Ejercicio 9
-- La siguiente gramática es una simplificación de la declaración de tipos en C
-- 
-- declaration -> type_specifier declarator ';'
-- declarator -> '*' declarator | direct_declarator
-- direct_declarator -> direct_declarator '[' constant_expression ']' | '(' direct_declarator ')' direct_declarator_ | identifier direct_declarator_
-- type_specifier -> 'int' | 'char' | 'float'
-- constant_expression -> number
-- 
-- Se va a construir un parser para esta gramática y se dará los tipos adecuados para representar estas declaraciones
-- Primero debemos eliminar la recursión a izquierda detectada

-- direct_declarator ->' (' direct_declarator ')' direct_declarator_ | identifier direct_declarator_
-- direct_declarator_ -> '[' constant_expression ']' direct_declarator_ | {empty} 

data ASTOfC = Decl TypeSpecifier Declarator
              deriving (Show, Eq)

data TypeSpecifier = TInt | TChar | TFloat
              deriving (Show, Eq)

data Declarator = Pointer Declarator | DirectDeclarator DirectDeclarator
              deriving (Show, Eq)

data DirectDeclarator = Array DirectDeclarator Int | Par DirectDeclarator | Identifier String
              deriving (Show, Eq)


declaration :: Parser ASTOfC
declaration = do t <- type_specifier
                 d <- declarator
                 symbol ";"
                 return (Decl t d)

declarator :: Parser Declarator
declarator = do symbol "*"
                d <- declarator
                return (Pointer d)
               <|> do dd <- directdeclarator 
                      return (DirectDeclarator dd)

directdeclarator :: Parser DirectDeclarator
directdeclarator = do symbol "("
                      dd <- directdeclarator
                      symbol ")"
                      dd_ <- directdeclarator_ (Par dd)
                      return dd_
                      <|> do i <- identifier
                             dd_ <- directdeclarator_ (Identifier i)
                             return dd_

directdeclarator_ :: DirectDeclarator -> Parser DirectDeclarator
directdeclarator_ d = do symbol "["
                         n <- int
                         symbol "]"
                         return (Array d n)
                        <|> return d

type_specifier :: Parser TypeSpecifier
type_specifier = do word "Int"
                    return TInt
                  <|> do word "Char"
                         return TChar
                        <|> do word "Float"
                               return TFloat  


-- Ejercicio 8
-- Transformar la gramática para eliminar la recursión izquierda e implementar el parser expr8 :: Parse Expr8 para la gramática transformada
--
-- expr -> expr ( '+' term | '-' term) | term
-- term -> term ('*' factor | '/' factor) | factor
-- factor -> digit | '(' expr ')'
-- digit -> '0' | '1' | ... | '9'

-- Gramátia sin recursión a izquierda
-- expr8 -> term8 expr8_
-- expr8_ -> '+' term8 expr8_ | '-' term8 expr8_ | {empty}
-- term8 -> factor8 term8_
-- term8_ -> '*' factor8 term8_ | '/' factor8 term8_ | {empty}
-- factor8 -> digit | '(' expr8 ')'

factor8 :: Parser Expr
factor8 = do i <- integer
             return (Num i)
            <|> do symbol "("
                   e <- expr8
                   symbol ")"
                   return e

term8 :: Parser Expr
term8 = do f <- factor8
           t <- term8_ f
           return t

term8_ :: Expr -> Parser Expr
term8_ t = do symbol "*"
              f <- factor8
              t' <- term8_ (BinOp Mul t f)
              return t'
             <|> do symbol "/"
                    f <- factor8
                    t' <- term8_ (BinOp Div t f)
                    return t'
                   <|> return t

expr8 :: Parser Expr
expr8 = do t <- term8
           e <- expr8_ t
           return e
       
expr8_ :: Expr -> Parser Expr
expr8_ e = do symbol "+"
              t <- term8
              e' <- expr8_ (BinOp Add e t)
              return e'
             <|> do symbol "-"
                    t <- term8
                    e' <- expr8_ (BinOp Min e t)
                    return e'
                   <|> return e