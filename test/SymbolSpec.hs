{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, NamedFieldPuns #-}
module SymbolSpec (spec, Ends, fromValid, toValid) where
import Symbol

import TestUtils
import Test.QuickCheck (property, Property, Arbitrary(..))
import Test.QuickCheck.Exception (discard)
import Test.QuickCheck.Monadic (monadicIO, pre)
import qualified Test.QuickCheck.Monadic as M
import GrammarSpec ()
import Grammar (Node)
import qualified Data.Vector as V
import Data.Vector (Vector, snoc)
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
  breakAtEndline' :: Ends -> EMG (Vector Node, Vector Symbol)
  breakAtEndline' = breakAtEndline . toValid
  unBreakAtEndline' :: (Vector Node, Vector Symbol) -> EMG Ends
  unBreakAtEndline' = fmap fromValid . unBreakAtEndline
  testIsNodeUnsafeToNode :: Symbol -> Property
  testIsNodeUnsafeToNode x = monadicIO
    $ do
      (pre . isNode) x
      let r = (tryEM . unsafeToNode) x
      (M.assert . isRight) r

newtype Ends = End {raw :: Vector Symbol}
  deriving (Arbitrary, Eq)
    
fromValid :: Vector Symbol -> Ends
fromValid v = 
  if V.last v == endline
    then End (V.init v)
    else error $ "Last Symbol is " ++ show (V.last v) ++ "not endline"
toValid :: Ends -> Vector Symbol
toValid End {raw} = raw `snoc` endline

instance Show Ends where
  show = (\s -> "End " ++ s ++ " \\End") . show . toValid