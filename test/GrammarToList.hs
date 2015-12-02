module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import qualified Data.Vector as V
import Grammar
import Test.QuickCheck

data Symbol = Term' Term | NonTerm' NonTerm | Break

instance Arbitrary Symbol where
  arbitrary = sized mkSymbol

grammarToList :: Grammar -> Vector Symbol
grammarToList = V.concatMap (V.snoc Break . (toSymbol `fmap`))

toSymbol :: Either NonTerm Term -> Symbol
toSymbol (Left l) = NonTerm' l
toSymbol (Right r) = Term' r

listToGrammar :: Vector Symbol -> Grammar
listToGrammar = go
  where
  go l =
    if V.null l
        then
          []
        else
          let (next , rest) = V.break (/= Break) l in
              next `V.cons` go (drop 1) rest
