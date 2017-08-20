{-# LANGUAGE TypeApplications, DeriveAnyClass, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving #-}
module GrammarSpec (module Grammar, spec) where
import Test.QuickCheck
import Grammar (Grammar(..), Node(..), Term, NonTerm)
import TestUtils (Spec)
import Control.Applicative ((<$>))

spec :: Spec
spec = pure ()

instance Arbitrary Node where
  arbitrary = oneof [TermNode <$> arbitrary, NonTermNode <$> arbitrary]

instance Arbitrary Grammar where
  arbitrary = Grammar <$> arbitrary <*> arbitrary
  shrink (Grammar a b) = drop 1 $ Grammar <$> shrink' a <*> shrink' b
    where
      shrink' x = x : shrink x
