{-# LANGUAGE
   FlexibleContexts
  #-}
module Lib
    ( compress
    , decompress
    , module Utils
    ) where
import qualified B
import qualified T
import BuildGrammar
import GrammarToList
import PadicEncode
import Control.Monad
import Control.Monad.Exception
import Utils

compress :: T.Text -> EM AnyException B.ByteString
compress = encode <=< grammarToList <=< buildGrammar

decompress :: B.ByteString -> EM AnyException T.Text
decompress = inflateGrammar <=< listToGrammar <=< decode
