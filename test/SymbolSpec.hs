{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module SymbolSpec where
import Symbol

import TestUtils
import Test.QuickCheck (property, Arbitrary)
import GrammarSpec

deriving instance Arbitrary Symbol

spec :: Spec
spec = describe "fromSymbol" $ it "undoes toSymbol" $ property prop_Symbol_from_to

prop_Symbol_from_to x = fromSymbol (toSymbol x) == Just x
