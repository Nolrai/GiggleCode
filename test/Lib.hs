module Lib
    ( compress
    , decompress
    ) where
import qualified Data.ByteString as B
import qualified Data.Text as T
import BuildGrammar
import GrammarToList

compress :: T.Text -> B.ByteString
compress = padicEncode . grammarToList . buildGrammar

decompress :: B.ByteString -> T.Text
decompress = inflateGrammar . listToGrammar . decodePadic
