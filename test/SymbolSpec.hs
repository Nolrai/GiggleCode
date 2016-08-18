module SymbolSpec where
import Symbol

import TestUtils

spec = property prop_Symbol_from_to

prop_Symbol_from_to x := fromSymbol (toSymbol x) = Just x
