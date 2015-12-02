module Grammar
    ( Grammar
    , Line
    , NonTerm (..)
    , Term (..)
    ) where
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.QuickCheck as Q

newtype Term = Term {toChar :: Char}
  deriving (Read, Show, Eq, Ord, Generic, Arbitrary)
newtype NonTerm = NonTerm {name :: Int}
  deriving (Read, Show, Eq, Ord, Arbitrary)

type Line = Vector (Either NonTerm Term)

instance Arbitrary Line where
  arbitrary = sized f
    where
    f n =
      do
      m <- frequency $ V.toList $ V.zip (V.enumFromStepN n (-1) n) (V.enumFromN 2 n)
      V.fromList `fmap` Q.vectorOf n (e n)
    e n = oneof [Left `fmap` arbitrary, (Right . NonTerm) `fmap` choose (1, n)]
    shrink = (V.fromList `fmap`) . shrink . (V.toList `fmap`)

type Grammar = Vector Line

instance Arbitrary Grammar where
  arbitrary = sized (\ n -> V.fromList (Q.vector n))
  shrink a = (V.filter isValid . V.fromList) `map` shrink $ V.toList a

