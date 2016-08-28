module Lib
    ( compress
    , decompress
    ) where
import qualified B
import qualified T
import BuildGrammar
import GrammarToList
import PadicEncode

compress :: T.Text -> B.ByteString
compress = encode . grammarToList . buildGrammar

decompress :: B.ByteString -> T.Text
decompress = inflateGrammar . listToGrammar . decode
