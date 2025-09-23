module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import GHC.Generics (Par1)

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
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until", "case"]
    , reservedOpNames = [ "+"
                        , "++"
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

intexp :: Parser (Exp Int)
intexp = intterm `chainl1` intOpterms

intOpterms :: Parser (Exp Int -> Exp Int -> Exp Int)
intOpterms = do reservedOp lis "+"
                return Plus
              <|> do reservedOp lis "-"
                     return Minus

intterm :: Parser (Exp Int)
intterm = intfactor `chainl1` intOpfactors

intOpfactors :: Parser (Exp Int -> Exp Int -> Exp Int)
intOpfactors = do reservedOp lis "*"
                  return Times
                 <|> do reservedOp lis "/"
                        return Div

intfactor :: Parser (Exp Int)
intfactor = do reservedOp lis "("
               e <- intexp
               reservedOp lis ")"   
               return e
              <|> do n <- natural lis
                     return (Const (fromInteger n))
                  <|> do v <- identifier lis
                         return (Var v)
                        <|> do reservedOp lis "-"
                               v <- identifier lis
                               return (UMinus (Var v))
                            --   <|> do v <- identifier lis
                            --          reservedOp lis "++"
                            --          return (VarInc v)
------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = boolterm `chainl1` boolOpterms

boolOpterms :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolOpterms = do reservedOp lis "||"
                 return Or
               <|> do reservedOp lis "&&"
                      return And

boolterm :: Parser (Exp Bool)
boolterm = do reserved lis "true"
              return BTrue
             <|> do reserved lis "false"
                    return BFalse
                   <|> do reservedOp lis "!"
                          Not <$> boolterm
                         <|> do reservedOp lis "("
                                boolexp <* reservedOp lis ")"
                               <|> try boolRelexp

boolRelexp :: Parser (Exp Bool)
boolRelexp = do e1 <- intexp
                do reservedOp lis "<"
                   Lt e1 <$> intexp
                  <|> do reservedOp lis ">"
                         Gt e1 <$> intexp
                        <|> do reservedOp lis "=="
                               Eq e1 <$> intexp
                              <|> do reservedOp lis "!="
                                     NEq e1 <$> intexp
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = commterm `chainl1` seqOpterms

seqOpterms :: Parser (Comm -> Comm -> Comm)
seqOpterms = do reservedOp lis ";"
                return Seq

commterm :: Parser Comm
commterm = (do reservedOp lis "if"
               b <- boolexp
               reservedOp lis "then"
               c1 <- comm
               do reservedOp lis "else"
                  IfThenElse b c1 <$> comm
                 <|> return (IfThen b c1))
               <|> do reservedOp lis "repeat"
                      c <- comm
                      reservedOp lis "until"
                      RepeatUntil c <$> boolexp
                     <|> do v <- identifier lis
                            reservedOp lis "="
                            Let v <$> intexp
                           <|> do reservedOp lis "("
                                  c <- comm
                                  reservedOp lis ")"
                                  return c
                                 <|> do reservedOp lis "skip"
                                        return Skip
                                       <|>  do reservedOp lis "case"
                                               reservedOp lis "{"
                                               cs <- many casebranches
                                               reservedOp lis "}"
                                               return (Case cs)
                                               
casebranches :: Parser (Exp Bool, Comm)
casebranches = do b <- boolexp
                  reservedOp lis ":"
                  reservedOp lis "{"
                  c <- comm
                  reservedOp lis "}"
                  return (b, c)
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
