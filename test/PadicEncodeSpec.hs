module PadicEncodeSpec
    ( spec
    ) where

import PadicEncode 
import TestUtils
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import Test.QuickCheck (Arbitrary)

type TestType = Char

encode_ :: V.Vector TestType -> B.ByteString
decode_ :: B.ByteString -> V.Vector TestType

encode_ = encode
decode_ = decode

spec :: Spec
spec = areInverses ("Padic encode", encode_) ("Padic decode", decode_)

