module TestUtils (module Test.Hspec, areInverses, isInverseOf, module B) where

import Data.List as L
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import Data.Text as T

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

areInverses (fname, f) (gname, g) =
  do
  (fname, f) `isInverseOf` (gname, g)
  (gname, g) `isInverseOf` (fname, f)

isInverseOf (fname, f) (gname, g) =
  describe fname $ it ("undoes " ++ gname) $ property (isId (g .> f))

isId f x = f x == x

f .> g = g . f
