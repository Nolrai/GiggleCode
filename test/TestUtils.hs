module TestUtils (module Test.Hspec, areInverses, isInverseOf) where

import Test.Hspec
import Test.QuickCheck

areInverses (fname, f) (gname, g) =
	do
	(fname, f) `isInverseOf` (gname, g)
	(gname, g) `isInverseOf` (fname, f)

isInverseOf (fname, f) (gname, g) =
	describe fname $ it ("undoes " ++ gname) $ property (isId (g .> f))

isId f x = f x == x

f .> g = g . f
