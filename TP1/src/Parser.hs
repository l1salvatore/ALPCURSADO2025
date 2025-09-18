module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
-- intexp ::= intexp1 intexp1_tail
intexp :: Parser (Exp Int)
intexp = do e <- intexp1
            f <- intexp1_tail
            return (f e)

-- intexp1 ::= intexp2 intexp1_tail
intexp1 :: Parser (Exp Int)
intexp1 = do left <- intexp2
             f <- intexp1_tail 
             return (f left)

-- intexp1_tail ::= "+" intexp2 intexp1_tail
--               | "-" intexp2 intexp1_tail
--               | ε
intexp1_tail :: Parser (Exp Int -> Exp Int)
intexp1_tail = do reservedOp lis "+"
                  right <- intexp2
                  f <- intexp1_tail
                  return (\x -> f (Plus x right))
                 <|> do reservedOp lis "-"
                        right <- intexp2
                        f <- intexp1_tail
                        return (\x -> f (Minus x right))
                       <|> return id

-- intexp2 ::= intexp3 intexp2_tail
intexp2 :: Parser (Exp Int)
intexp2 = do left <- intexp3
             f <- intexp2_
             return (f left)

-- intexp2_tail ::= "*" intexp3 intexp2_tail
--               | "/" intexp3 intexp2_tail
--               | ε
intexp2_ :: Parser (Exp Int -> Exp Int)
intexp2_ = do reservedOp lis "*"
              right <- intexp3
              f <- intexp2_
              return (\x -> f (Times x right))
             <|> do reservedOp lis "/"
                    right <- intexp3
                    f <- intexp2_
                    return (\x -> f (Div x right))
                   <|> return id

-- intexp3 ::= natural
--          | identifier
--          | "-" intexp3
--          | "(" intexp ")"
--          | identifier "++"
--          | intexp

intexp3 :: Parser (Exp Int)
intexp3 = do n <- natural lis
             return (Const (fromInteger n))
            <|> do v <- identifier lis
                   return (Var v)
            <|> do reservedOp lis "-"
                   UMinus <$> intexp3
            <|> do reservedOp lis "("
                   e <- intexp
                   reservedOp lis ")"
                   return e
            <|> do v <- identifier lis
                   reservedOp lis "++"
                   return (VarInc (Var v))
                  <|> intexp


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
