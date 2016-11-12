{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module SymbolSpec where
import Symbol

import TestUtils
import Test.QuickCheck (property, Arbitrary(..), (==>))
import GrammarSpec ()
import Grammar (Node)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Exception (throw)
import Control.Applicative ((<$>),(<*>))

deriving instance Arbitrary Symbol

doesThrow = isRight . tryEvaluate . force

spec :: Spec
spec =
  do
  isInverseOf
    ("unsafeToNode", unsafeToNode)
    ("toSymbol", toSymbol)
  it "isNode allows unsafeToNode"
    $ property (\ x  -> isNode x == (doesThrow $ unsafetoNode x))
  areInverses
    ("breakAtEndline", breakAtEndline')
    ("unBreakAtEndline", unBreakAtEndline')
  where
  breakAtEndline' :: Valid -> (Vector Node, Vector Symbol)
  breakAtEndline' = breakAtEndline . fromValid
  unBreakAtEndline' :: (Vector Node, Vector Symbol) -> Valid a
  unBreakAtEndline' = unsafeMkValid . unBreakAtEndline

instance Arbitrary Valid where
  arbitrary = Valid <$> arbitrary <*> arbitrary
  shrink = genericShrink

data Valid = Valid (Vector Node) (Vector Symbol)
  deriving (Show, Read, Eq, Generic)

unsafeMkValid vec e =
  if any (== endline) vec
    then Valid vec
    else throw e
