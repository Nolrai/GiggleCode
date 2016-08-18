module LibSpec
    ( spec
    ) where
import Lib 
import TestUtils

spec :: Spec
spec = areInverses ("compress", compress) ("decompress", decompress)

