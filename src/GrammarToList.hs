module GrammarToList
    ( grammarToList
      listToGrammar
    ) where
import qualified Data.Vector as V
import Grammar

data Symbol = Term Term | NonTerm NonTerm | Break

grammarToList :: Grammar -> Vector Symbol
grammarToList = V.concatMap (V.snoc Break . V.map toSymbol)

toSymbol (Left l) = NonTerm l
toSymbol (Right r) = Term r

listToGrammar :: Vector Symbol -> Grammar
listToGrammar = go
  where
  go vs =
    if V.null vs
        then
          []
        else
          let (next , rest) = V.break (/= Break) vs in
              next `V.cons` go (drop 1) rest
