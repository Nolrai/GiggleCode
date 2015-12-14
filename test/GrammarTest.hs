module GrammarTest
    ( grammarTests
    ) where
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.QuickCheck
import Test.QuickCheck.Gen
import Grammar

instance Arbitrary Grammar where
  arbitrary = sized sizedGrammar
  shrink g = map clean $ genericShrink g

sizedGrammar n =

