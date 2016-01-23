module Lib
    ( compress
    , decompress
    ) where
import ByteString as B
import Data.Text (Text)
import BuildGrammar
import GrammarToList
import PadicEncode

compress :: B.ByteString -> B.ByteString
compress = padicEncode . undefined . buildGrammar

decompress :: B.ByteString -> B.ByteString
decompress = inflateGrammar . undefined  . padicDecode
