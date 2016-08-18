module PadicEncodeSpec
    ( spec
    ) where

import PadicEncode (encode, decode)
import TestUtils

spec :: Spec
spec = areInverses ("Padic encode", encode) ("Padic decode", decode)

