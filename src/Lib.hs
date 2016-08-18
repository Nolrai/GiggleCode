module Lib
    ( compress
    , decompress
    ) where
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import Data.Text (Text)
import BuildGrammar
import GrammarToList
import PadicEncode

compress :: Text -> B.ByteString
compress = encode . grammarToList . buildGrammar

decompress :: B.ByteString -> Text
decompress = inflateGrammar . listToGrammar . decode
