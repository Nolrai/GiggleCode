{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
           , ConstraintKinds
 #-}
module TestUtils (module Test.Hspec, areInverses, isInverseOf, (.>), EMG, (<=<), (>=>)) where

import Data.List as L
import Utils (EMG)
import Test.Hspec
import Test.QuickCheck
import qualified B
import qualified T
import qualified Data.Vector as V
import Control.Monad.Exception
import Control.Monad ((>=>), (<=<))

instance Arbitrary T.Text where
  arbitrary = T.pack `fmap` arbitrary
  shrink = L.map T.pack . shrink . T.unpack

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary
  shrink = L.map B.pack . shrink . B.unpack

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary =
    do
    n <- arbitrarySizedNatural
    V.replicateM n arbitrary
  shrink = L.map V.fromList . shrinkList shrink . V.toList

type Testible a = (Arbitrary a, Show a, Eq a)

areInverses :: (Testible a, Testible b)
  => (String, a -> EMG b)
  -> (String, b -> EMG a)
  -> Spec
areInverses (fname, f) (gname, g) =
  do
  (fname, f) `isInverseOf` (gname, g)
  (gname, g) `isInverseOf` (fname, f)

isInverseOf :: (Testible a, Testible b)
  => (String, a -> EMG b)
  -> (String, b -> EMG a)
  -> Spec
isInverseOf (fname, f) (gname, g) =
  describe fname $ it ("undoes " ++ gname) $ property (isId (g >=> f))

isId :: (Eq a, Show a) => (a -> EMG a) -> a -> Expectation
isId f x =
  case tryEMWithLoc $ shouldBe x <$> f x of
    Right expectation -> expectation
    Left (trace, e) -> error $ showExceptionWithTrace trace e

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = g . f
