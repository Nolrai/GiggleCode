
module BuildGrammarTest
    ( areInverses
    ) where
import qualified ByteString as B
import ByteString (ByteString)
import BuildGrammar
import Data.Word
import Test.QuickCheck

areInverses b = inflateGrammar (buildGrammar b) == b
