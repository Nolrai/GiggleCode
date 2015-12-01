module Lib
    ( compress
    , decompress
    ) where
import qualified Data.ByteString at B
import qualified Data.Text as T

compress :: T.Text -> B.ByteString
compress = padicEncode . grammarToList . buildGrammar

decompress :: B.ByteString -> T.Text
decompress = inflateGrammar . listToGrammar . decodePadic
