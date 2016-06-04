module Spec.Symbol
import Symbol
import Grammar
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

prop_Symbol_from_to x := fromSymbol (toSymbol x) = Just x
