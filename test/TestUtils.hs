module TestUtils (module Test.Hspec, areInverses, isInverseOf) where

import Data.List as L
import Test.Hspec
import Test.QuickCheck
import qualified B
import qualified T
import qualified Data.Vector as V

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

areInverses
  :: (Arbitrary a, Arbitrary b, Show a, Show b, Eq a, Eq b)
  => (String, (a -> b))
  -> (String, (b -> a))
  -> Spec
areInverses (fname, f) (gname, g) =
  do
  (fname, f) `isInverseOf` (gname, g)
  (gname, g) `isInverseOf` (fname, f)

isInverseOf
  :: (Arbitrary a, Arbitrary b, Show a, Show b, Eq a, Eq b)
  => (String, (a -> b))
  -> (String, (b -> a))
  -> Spec
isInverseOf (fname, f) (gname, g) =
  describe fname $ it ("undoes " ++ gname) $ property (isId (g .> f))

isId :: (Eq a, Show a) => (a -> a) -> a -> Expectation
isId f x = f x `shouldBe` x

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f
