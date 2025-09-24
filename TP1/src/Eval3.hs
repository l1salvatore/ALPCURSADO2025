module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (m, s)= case (M.lookup v m) of
                      Nothing -> Left UndefVar
                      Just n -> Right n

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v i (m, s)= (M.insert v i m, s)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace s' (m, s)= (m, s++" "++s')

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip state = Right (Skip :!: state)
stepComm (Let v e) state = case evalExp e state of
                           Left err -> Left err
                           Right (n :!: s') -> Right (Skip :!: update v n (addTrace ("Let "++v++" "++show n) s'))
stepComm (Seq Skip c1) s = stepComm c1 s
stepComm (Seq c0 c1) s = case stepComm c0 s of
                              Left err -> Left err
                              Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')
stepComm (IfThenElse b c0 c1) s = case evalExp b s  of
                                    Left err -> Left err
                                    Right (bv :!: s') -> Right (if bv then c0 :!: s' else c1 :!: s')
stepComm (RepeatUntil c b) s = Right ( Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var x) s = case lookfor x s of
                        Left err -> Left err
                        Right n -> Right (n :!: s)
evalExp (UMinus e) s = case evalExp e s of
                        Left err -> Left err
                        Right (n :!: s')  -> Right ((-n) :!: s')
evalExp (Plus e1 e2) s = case evalExp e1 s of
                        Left err -> Left err
                        Right (n0 :!: s')  ->
                            case evalExp e2 s' of
                                Left err -> Left err
                                Right (n1 :!: s'') -> Right (n0 + n1 :!: s'')
evalExp (Minus e1 e2) s = case evalExp e1 s of
                          Left err -> Left err
                          Right (n0 :!: s')  ->
                              case evalExp e2 s' of
                                  Left err -> Left err
                                  Right (n1 :!: s'') -> Right (n0 - n1 :!: s'')
evalExp (Times e1 e2) s =case evalExp e1 s of
                        Left err -> Left err
                        Right (n0 :!: s')  ->
                            case evalExp e2 s' of
                                Left err -> Left err
                                Right (n1 :!: s'') -> Right (n0 * n1 :!: s'')
evalExp (Div e1 e2) s =case evalExp e1 s of
                        Left err -> Left err
                        Right (n0 :!: s')  ->
                            case evalExp e2 s' of
                                Left err -> Left err
                                Right (n1 :!: s'') -> if n1 == 0 then Left DivByZero else Right (div n0 n1 :!: s'')
evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (Eq e1 e2) s = case evalExp e1 s of
                        Left err -> Left err
                        Right (n0 :!: s')  ->
                            case evalExp e2 s' of
                                Left err -> Left err
                                Right (n1 :!: s'') -> Right (n0 == n1 :!: s'')
evalExp (Lt e1 e2) s = case evalExp e1 s of
                        Left err -> Left err
                        Right (n0 :!: s')  ->
                            case evalExp e2 s' of
                                Left err -> Left err
                                Right (n1 :!: s'') -> Right (n0 < n1 :!: s'')
evalExp (Gt e1 e2) s = case evalExp e1 s of
                        Left err -> Left err
                        Right (n0 :!: s')  ->
                            case evalExp e2 s' of
                                Left err -> Left err
                                Right (n1 :!: s'') -> Right (n0 > n1 :!: s'')
evalExp (NEq e1 e2) s = case evalExp e1 s of
                        Left err -> Left err
                        Right (n0 :!: s')  ->
                            case evalExp e2 s' of
                                Left err -> Left err
                                Right (n1 :!: s'') -> Right (n0 /= n1 :!: s'')
evalExp (Not p) s = case evalExp p s of
                        Left err -> Left err
                        Right (b :!: s')  -> Right (not b :!: s')
evalExp (Or p1 p2) s = case evalExp p1 s of
                        Left err -> Left err
                        Right (b1 :!: s')  ->
                            case evalExp p2 s' of
                                Left err -> Left err
                                Right (b2 :!: s'') -> Right ((b1 || b2) :!: s'')
evalExp (And p1 p2) s =  case evalExp p1 s of
                        Left err -> Left err
                        Right (b1 :!: s')  ->
                            case evalExp p2 s' of
                                Left err -> Left err
                                Right (b2 :!: s'') -> Right ((b1 && b2) :!: s'')
evalExp (VarInc v) s = Right (0 :!: s)
