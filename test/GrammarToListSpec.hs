module GrammarToListSpec
    ( spec
    ) where

import GrammarToList
    ( grammarToList
    , listToGrammar
    )
import TestUtils

spec :: Spec
spec = 
  areInverses
    ("grammarToList", grammarToList)
    ("listToGrammar", listToGrammar)
