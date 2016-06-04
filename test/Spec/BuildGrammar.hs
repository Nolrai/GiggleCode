module Spec.BuildGrammar
     (tests)
     where
import qualified Data.Text as T
import Grammar
import BuildGrammar
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

tests = areInverses ("build", buildGrammar) ("inflate", inflateGrammer) 
