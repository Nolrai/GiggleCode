{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module SymbolSpec where
import Symbol

import TestUtils
import Test.QuickCheck (property, Arbitrary, (==>))
import GrammarSpec ()
import Grammar ()

deriving instance Arbitrary Symbol

isValue :: Eq a => a -> Bool
isValue x = if x == x then True else (error "isValue: x == x is false.")

spec :: Spec
spec =
  do
  isInverseOf ("unsafeToNode", unsafeToNode) ("toSymbol", toSymbol)
  it "isNode allows unsafeToNode" $ property (\ x  -> isNode x ==> isValue (unsafeToNode x) )
  areInverses ("breakAtEndline", breakAtEndline) ("unBreakAtEndline", unBreakAtEndline)
