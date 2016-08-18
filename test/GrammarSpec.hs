{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module GrammarSpec where
import Test.QuickCheck (Arbitrary)
import Grammar
import Test.Hspec

spec :: Spec
spec = return ()

deriving instance Arbitrary NonTerm
deriving instance Arbitrary Term
deriving instance Arbitrary Node
