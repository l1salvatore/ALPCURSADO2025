import Parsing
import Control.Applicative
import GHC.Internal.Read (paren)
import Text.XHtml (base, ddef)
import Data.Char
import CustomParsing
import Data.Sequence (Seq(Empty))
import Data.Kind (Type)

-- Pensando el TP
-- ¿DSL para definir un modelo o facilitar la construcción de escape rooms?