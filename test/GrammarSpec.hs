{-# LANGUAGE TypeApplications, DeriveAnyClass, FlexibleInstances #-}
module GrammarSpec (module Grammar, spec) where
import Test.QuickCheck
import Grammar (Grammar(..), Node(..), Term, NonTerm, Line)
import TestUtils (Spec)
import Data.Vector (cons, Vector)
import Control.Applicative ((<$>))

spec :: Spec
spec = pure ()

instance Arbitrary Node where
  arbitrary = oneof [TermNode <$> arbitrary, NonTermNode <$> arbitrary]

mkLine :: Gen Line
mkLine = f <$> arbitrary <*> arbitrary <*> scale (\x -> x - 2) arbitrary
  where
  f a b c = a `cons` (b `cons` c)

mkRules :: Gen (Vector Line)
mkRules =
  do
  v <- arbitrary :: Gen (Vector ())
  sequence $ fmap (const mkLine) v

instance Arbitrary Grammar where
  arbitrary = Grammar <$> mkLine <*> mkRules
  shrink (Grammar a b) = drop 1 $ Grammar <$> shrink' a <*> shrink' b
    where
      shrink' x = x : shrink x
