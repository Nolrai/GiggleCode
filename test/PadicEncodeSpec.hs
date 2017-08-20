{-# LANGUAGE FlexibleContexts #-}
module PadicEncodeSpec
    ( spec
    ) where
import TestUtils
{-
import PadicEncode
import Utils (Stub)

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import Control.Monad.Exception (EM, Throws)
import Test.QuickCheck ()

type TestType = Char


encode_ :: (Throws Stub l) => V.Vector TestType -> EM l B.ByteString
decode_ :: (Throws Stub l) => B.ByteString -> EM l (V.Vector TestType)

encode_ = encode
decode_ = decode

spec :: Spec
spec = areInverses ("Padic encode", encode_) ("Padic decode", decode_)
-}
spec :: Spec
spec = pure ()

