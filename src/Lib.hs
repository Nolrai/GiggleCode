module Lib
    ( compress
    , decompress
    ) where
import qualified B
import qualified T
import BuildGrammar
import GrammarToList
import PadicEncode
import Control.Monad
import Control.Monad.Exception

compress = encode <=< grammarToList <=< buildGrammar

decompress = inflateGrammar <=< listToGrammar <=< decode
