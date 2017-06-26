{-# LANGUAGE StandaloneDeriving, DeriveGeneric, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module SymbolSpec (spec) where
import Symbol

import TestUtils
import Test.QuickCheck (property, Property, Arbitrary(..))
import Test.QuickCheck.Exception (discard)
import Test.QuickCheck.Monadic (monadicIO, pre)
import qualified Test.QuickCheck.Monadic as M
import GrammarSpec ()
import Grammar (Node)
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Control.Exception as E
import Control.Applicative ((<$>),)
import GHC.Generics
import Control.Monad.Exception

deriving instance Arbitrary Symbol

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

spec :: Spec
spec =
  do
  isInverseOf
    ("unsafeToNode", unsafeToNode)
    ("toSymbol", toSymbol .> return)
  it "isNode allows unsafeToNode"
    $ property testIsNodeUnsafeToNode
  areInverses
    ("breakAtEndline", breakAtEndline')
    ("unBreakAtEndline", unBreakAtEndline')
  where
  breakAtEndline' :: Valid -> EMG (Vector Node, Vector Symbol)
  breakAtEndline' = breakAtEndline . fromValid
  unBreakAtEndline' :: (Vector Node, Vector Symbol) -> EMG Valid
  unBreakAtEndline' = unsafeMkValid . unBreakAtEndline
  testIsNodeUnsafeToNode :: Symbol -> Property
  testIsNodeUnsafeToNode x = monadicIO
    $ do
      (pre . isNode) x
      let r = (tryEM . unsafeToNode) x
      (M.assert . isRight) r

instance Arbitrary Valid where
  arbitrary = discardInvalid <$> arbitrary
  shrink (Valid v) = discardInvalid <$> shrink v

discardInvalid :: Vector Symbol -> Valid
discardInvalid v =
  if V.all (/= endline) v
    then discard
    else Valid v

data Valid = Valid {fromValid :: Vector Symbol}
  deriving (Show, Eq, Generic)

unsafeMkValid :: Vector Symbol -> EMG Valid
unsafeMkValid v =
  if (any (== endline) v)
    then (E.throw UnsafeToNodeEndline)
    else pure $ Valid v

