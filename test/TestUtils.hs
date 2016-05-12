module TestUtils where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

areInverses (fname, f) (gname, g) =
	testGroup
	(fname ++ " " ++ gname ++ " are inverses.")
	[ (fname, f) `isInverseOf` (gname, g)
	, (gname, g) `isInverseOf` (fname, f)
	]

isInverseOf (fname, f) (gname, g) =
	testProperty (fname ++ " then " gname) (isId (f .> g))

isId f x = f x == x
