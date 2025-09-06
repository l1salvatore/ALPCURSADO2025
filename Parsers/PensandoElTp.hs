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
-- objectlist -> Object name '{' objectlist '}' (',' objectlist | {{ empty }}) 

data EscapeRoom = EscapeRoom [Object] EscapeDoor
    deriving (Eq, Show)
data EscapeDoor = EscapeDoor  UnlockMode
    deriving (Eq, Show)

data UnlockMode = Key Int | Code String | Button Int
    deriving (Eq, Show)

data Object = Object String [Object] | None
    deriving (Eq, Show)


objectlist :: Parser [Object]
objectlist = do symbol "Object"
                i <- identifier
                symbol "{"
                o <- objectlist
                symbol "}"
                do symbol ","
                   xs <- objectlist
                   return ((Object i o) : xs)
                  <|> return [Object i o]
              <|> return []

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