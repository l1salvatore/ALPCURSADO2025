import Parsing
import Control.Applicative
import GHC.Internal.Read (paren)
import Text.XHtml (base, ddef)
import Data.Char
import CustomParsing
import Data.Sequence (Seq(Empty))
import Data.Kind (Type)
import Distribution.Simple.PackageIndex (SearchResult(None))

-- Pensando el TP
-- ¿DSL para definir un modelo o facilitar la construcción de escape rooms?
--
--
-- pensamos la gramática
--
-- escaperoom -> objectlist ';' escapedoor
-- escapedoor -> 'EscapeDoor' unlockmode
-- unlockmode -> 'Key' integer | 'Code' string | 'Button' integer
-- objectlist -> object (',' objectlist | {{ empty }}) 
-- object -> 'Object' name | 'Openable' name unlockmode
                 

data EscapeRoom = EscapeRoom [Object] EscapeDoor
    deriving (Eq, Show)
data EscapeDoor = EscapeDoor  UnlockMode
    deriving (Eq, Show)

data UnlockMode = Key Int | Code String | Button Int
    deriving (Eq, Show)

data Object = Object String [Object] | Openable String UnlockMode [Object] | None
    deriving (Eq, Show)



objectlist :: Parser [Object]
objectlist = do o <- object
                do symbol ","
                   xs <- objectlist
                   return (o : xs)
                  <|> return [o]
              <|> return []

object :: Parser Object
object = do symbol "Object"
            i <- identifier
            symbol "{"
            o <- objectlist
            symbol "}"
            return (Object i o)
           <|> do symbol "Openable"
                  i <- identifier
                  u <- unlockmode
                  symbol "{"
                  o <- objectlist
                  symbol "}"
                  return (Openable i u o)

unlockmode :: Parser UnlockMode
unlockmode = do symbol "Key"
                i <- integer
                return (Key i)
               <|> do symbol "Code"
                      i <- identifier
                      return (Code i)
                     <|> do symbol "Button"
                            i <- integer
                            return (Button i)

escapedoor :: Parser EscapeDoor
escapedoor = do symbol "EscapeDoor"
                u <- unlockmode
                return (EscapeDoor u)

escaperoom :: Parser EscapeRoom
escaperoom = do o <- objectlist
                symbol ";"
                e <- escapedoor
                return (EscapeRoom o e)