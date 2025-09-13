import Parsing
import Control.Applicative
import Text.XHtml (base, ddef)
import Data.Char
import CustomParsing
import Data.Sequence (Seq(Empty))
import Data.Kind (Type)
import Distribution.Simple.PackageIndex (SearchResult(None))

-- Pensando el TP
-- ¿DSL para modelar cadenas de markov y realizar simulaciones?
-- Está pensado como máquinas de estados para realizar simulaciones de un sistema que evoluciona en el tiempo
-- Se lo podría configurar como un pequeño proceso que lanze eventos que cualquier otro proceso pueda escuchar
-- y actuar en consecuencia.
--
--
-- pensamos la gramática
--
-- MarkovChain -> "States" ":" "{" States "}" "Transitions" ":" Transitions "InitialDistribution" ":" InitialDistribution "Steps" ":" Steps
-- States -> State (',' States | {empty})
-- Transitions -> State '->' State ',' Prob ';' (Transitions | {empty})
-- InitialDistribution -> State ':' Prob ';' (InitialDistribution | {empty})
-- State -> identifier
-- Prob -> decimal
-- Steps -> integer

data MarkovChain = MC States Transitions InitialDistribution Steps
    deriving (Eq, Show)
type States = [State]
type Transitions = [Transition]
type InitialDistribution = [InitialProb]
type Steps = Int
type State = String
type Transition = (State, State, Double)
type InitialProb = (State, Double)




markovChain :: Parser MarkovChain
markovChain = do s <- states
                 t <- transitions
                 i <- initialDistribution
                 MC s t i <$> steps


states :: Parser States
states = do symbol "States"
            symbol ":"
            symbol "{"
            ss <- identifier `sepBy` symbol ","
            symbol "}"
            return ss

transitions :: Parser Transitions
transitions = do symbol "Transitions"
                 symbol ":"
                 transition `endBy` symbol ";"
    where transition = do from <- identifier
                          symbol "->"
                          to <- identifier
                          symbol ":"
                          p <- decimal
                          return (from, to, p)

initialDistribution :: Parser InitialDistribution
initialDistribution = do symbol "InitialDistribution"
                         symbol ":"
                         initialProb `endBy` symbol ";"
    where initialProb = do s <- identifier
                           symbol ":"
                           p <- decimal
                           return (s, p)

steps :: Parser Steps
steps = do symbol "Steps"
           symbol ":"
           integer
