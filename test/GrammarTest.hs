{-# LANGUAGE ScopedTypeVariables #-}
module GrammarTest
    ( grammarTests
    ) where
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Grammar
import Data.Maybe (isJust)

grammarTests :: Test
grammarTests = testGroup "Grammar Tests"
  [ testProperty "Arbitrary Grammar Verified" verifiesChar
  , testProperty "Arbitrary Grammar Verified" verifiesBool
  ]

verifiesBool :: Grammar Bool -> Bool
verifiesBool = verifies

verifiesChar :: Grammar Char -> Bool
verifiesChar = verifies

verifies :: Show a => Grammar a -> Bool
verifies = isJust . verifyGrammar

instance Arbitrary a => Arbitrary (Grammar a) where
  arbitrary = grammar
  shrink g = filter (isJust . verifyGrammar) $ genericShrink g

grammar :: Arbitrary a => Gen (Grammar a)
grammar = Grammar <$> longLine <*> scale pred rules
  where
  longLine =
   Line
    <$> node
    <*> node
    <*> sized (\n -> vectorOf (10 * n) node)

instance Arbitrary a => Arbitrary (Node a) where
  arbitrary = node
  shrink = genericShrink

node :: Arbitrary a => Gen (Node a)
node = sized sizedNode

sizedNode :: forall a. Arbitrary a => Int -> Gen (Node a)
sizedNode n = frequency [(alphabetSize, term), (n, nonterm)]
  where
  alphabetSize :: Int
  alphabetSize = 30
  term :: Gen (Node a)
  term = Term <$> resize alphabetSize arbitrary
  nonterm :: Gen (Node a)
  nonterm = (Nonterm . toEnum) <$> (choose (0,n - 1))

instance Arbitrary a => Arbitrary (Rules a) where
  arbitrary = rules
  shrink (Rules r) = Rules <$> shrink r

rules :: forall a. Arbitrary a => Gen (Rules a)
rules = Rules <$> sized sizedRules
sizedRules n
  | n < 0 = return []
  | otherwise = (:) <$> line <*> scale pred (unRules <$> rules)

unRules (Rules r) = r

instance Arbitrary a => Arbitrary (Line a) where
  arbitrary = line
  shrink = genericShrink

line :: forall a. Arbitrary a => Gen (Line a)
line = Line <$> node <*> node <*> listOf node
