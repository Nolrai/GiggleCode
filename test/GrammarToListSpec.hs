{-# LANGUAGE FlexibleInstances #-}
module GrammarToListSpec
    ( spec
    ) where

import GrammarToList
    ( grammarToList
    , listToGrammar
    )
import Prelude hiding (init)
import TestUtils
import Grammar
import SymbolSpec ()
import Symbol
import Data.Vector (init, Vector, snoc)
import Test.QuickCheck

newtype Valid v = Valid {fromValid :: Vector v}
  deriving (Eq, Ord)

instance Show v => Show (Valid v) where
  show (Valid x) = show x

mkValid :: Vector Symbol -> Valid Symbol
mkValid = Valid . (`snoc` Symbol Nothing)
unValid :: Valid Symbol -> Vector Symbol
unValid (Valid v) = init v

-- It's important that in the test functions its Valid/fromValid
-- and NOT mkValid/unValid because we are just using Valid to
-- over write the Arbitrary instance.
--        (which is what mkValid and unValid are for)
gltest :: Grammar.Grammar -> Valid Symbol
lgtest :: Valid Symbol -> Grammar.Grammar
gltest = Valid . grammarToList
lgtest = listToGrammar . fromValid

instance Arbitrary (Valid Symbol) where
  arbitrary = mkValid <$> arbitrary
  shrink = (mkValid <$>) . shrink . unValid

spec :: Spec
spec =
  do
  areInverses
    ("mkValid", mkValid)
    ("unValid", unValid)
  areInverses
    ("grammarToList", gltest)
    ("listToGrammar", lgtest)
