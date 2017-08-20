module LibSpec
    ( spec
    ) where
import Lib ({-compress, decompress-}) 
import TestUtils

spec :: Spec
spec = pure () -- areInverses ("compress", compress) ("decompress", decompress)

