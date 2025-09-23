module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of
                 Just n -> n
                 Nothing -> error ("Variable " ++ v ++ " no definida.")
                  
-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s -- Devuelve un warning si no pongo esto
stepComm (Let v e) s = let (n :!: s') = evalExp e s in Skip :!: update v n s'
stepComm (Seq Skip c1) s = c1 :!: s
stepComm (Seq c0 c1) s = let (c0' :!: s') = stepComm c0 s in Seq c0' c1 :!: s'
stepComm (IfThenElse b c0 c1) s = let (bv :!: s') = evalExp b s in 
                                  if bv then c0 :!: s' else c1 :!: s'
stepComm (RepeatUntil c b) s = Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s

  
  -- -- Expresiones enteras
  -- Const  :: Int -> Exp Int
  -- Var    :: Variable -> Exp Int
  -- UMinus :: Exp Int -> Exp Int
  -- Plus   :: Exp Int -> Exp Int -> Exp Int
  -- Minus  :: Exp Int -> Exp Int -> Exp Int
  -- Times  :: Exp Int -> Exp Int -> Exp Int
  -- Div    :: Exp Int -> Exp Int -> Exp Int
  -- VarInc :: Variable -> Exp Int -- x++

  -- -- Expresiones booleanas
  -- BTrue  :: Exp Bool
  -- BFalse :: Exp Bool
  -- Lt     :: Exp Int -> Exp Int -> Exp Bool
  -- Gt     :: Exp Int -> Exp Int -> Exp Bool
  -- And    :: Exp Bool -> Exp Bool -> Exp Bool
  -- Or     :: Exp Bool -> Exp Bool -> Exp Bool
  -- Not    :: Exp Bool -> Exp Bool
  -- Eq     :: Exp Int -> Exp Int -> Exp Bool
  -- NEq    :: Exp Int -> Exp Int -> Exp Bool  

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = n :!: s
evalExp (Var x) s = lookfor x s :!: s
evalExp (UMinus e) s = let (n :!: s') = evalExp e s in (-n) :!: s'
evalExp (Plus e1 e2) s = let (n0 :!: s') = evalExp e1 s
                             (n1 :!: s'') = evalExp e2 s'
                         in (n0 + n1 :!: s'')
evalExp (Minus e1 e2) s = let (n0 :!: s') = evalExp e1 s
                              (n1 :!: s'') = evalExp e2 s'
                          in (n0 - n1 :!: s'')
evalExp (Times e1 e2) s = let (n0 :!: s') = evalExp e1 s
                              (n1 :!: s'') = evalExp e2 s'
                          in (n0 * n1 :!: s'')
evalExp (Div e1 e2) s = let (n0 :!: s') = evalExp e1 s
                            (n1 :!: s'') = evalExp e2 s'
                        in (if n1 /= 0 then (div n0 n1 :!: s'') else error("Div by zero"))
evalExp (BTrue) s = True :!: s
evalExp (BFalse) s = False :!: s
evalExp (Eq e1 e2) s = let (n0 :!: s') = evalExp e1 s
                           (n1 :!: s'') = evalExp e2 s'
                       in (n0 == n1 :!: s'')
evalExp (Lt e1 e2) s = let (n0 :!: s') = evalExp e1 s
                           (n1 :!: s'') = evalExp e2 s'
                       in (n0 < n1 :!: s'')
evalExp (Gt e1 e2) s = let (n0 :!: s') = evalExp e1 s
                           (n1 :!: s'') = evalExp e2 s'
                       in (n0 > n1 :!: s'')
evalExp (NEq e1 e2) s = let (n0 :!: s') = evalExp e1 s
                            (n1 :!: s'') = evalExp e2 s'
                        in (n0 /= n1 :!: s'')
evalExp (Not p) s = let (b :!: s') = evalExp p s
                    in (not b :!: s')
evalExp (Or p1 p2) s = let (b1 :!: s') = evalExp p1 s
                           (b2 :!: s'') = evalExp p2 s'
                       in ((b1 || b2) :!: s'')                
evalExp (And p1 p2) s = let (b1 :!: s') = evalExp p1 s
                            (b2 :!: s'') = evalExp p2 s'
                        in ((b1 && b2) :!: s'')                
