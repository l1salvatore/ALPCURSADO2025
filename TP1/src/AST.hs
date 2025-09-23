{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones, aritméticas y booleanas
data Exp a where
  
  -- Expresiones enteras
  Const  :: Int -> Exp Int
  Var    :: Variable -> Exp Int
  UMinus :: Exp Int -> Exp Int
  Plus   :: Exp Int -> Exp Int -> Exp Int
  Minus  :: Exp Int -> Exp Int -> Exp Int
  Times  :: Exp Int -> Exp Int -> Exp Int
  Div    :: Exp Int -> Exp Int -> Exp Int
  VarInc :: Variable -> Exp Int -- x++

  -- Expresiones booleanas
  BTrue  :: Exp Bool
  BFalse :: Exp Bool
  Lt     :: Exp Int -> Exp Int -> Exp Bool
  Gt     :: Exp Int -> Exp Int -> Exp Bool
  And    :: Exp Bool -> Exp Bool -> Exp Bool
  Or     :: Exp Bool -> Exp Bool -> Exp Bool
  Not    :: Exp Bool -> Exp Bool
  Eq     :: Exp Int -> Exp Int -> Exp Bool
  NEq    :: Exp Int -> Exp Int -> Exp Bool  

deriving instance Show (Exp a)
deriving instance Eq (Exp a)

-- Comandos (sentencias)
-- Observar que sólo se permiten variables de tipo entero
data Comm
  = Skip
  | Let Variable (Exp Int)
  | Seq Comm Comm
  | IfThenElse (Exp Bool) Comm Comm
  | RepeatUntil Comm (Exp Bool)
  deriving (Show, Eq)

pattern IfThen :: Exp Bool -> Comm -> Comm
pattern IfThen b c = IfThenElse b c Skip
pattern Case :: [(Exp Bool, Comm)] -> Comm
pattern Case branches <- (seqListView -> branches)
  where
    Case branches = seqList branches

-- Helper function for the view pattern
seqListView :: Comm -> [(Exp Bool, Comm)]
seqListView Skip = []
seqListView (IfThenElse b c rest) = (b, c) : seqListView rest
seqListView _ = error "Not a Case sequence"

-- Helper to convert a sequence of IfThen commands into a list of branches
seqList :: [(Exp Bool, Comm)] -> Comm
seqList [] = Skip
seqList ((b, c):xs) = IfThenElse b c (seqList xs)


-- view helper
restFromSkip :: Comm -> Maybe Comm
restFromSkip Skip = Nothing
restFromSkip r    = Just r

data Error = DivByZero | UndefVar deriving (Eq, Show)
