module GrammarToListSpec
    ( spec
    ) where

import GrammarToList
    ( grammarToList
    , listToGrammar
    )
import TestUtils
import SymbolSpec ()

spec :: Spec
spec = 
  areInverses
    ("grammarToList", grammarToList)
    ("listToGrammar", listToGrammar)
