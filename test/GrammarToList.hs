module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import qualified Data.Vector as V
import Data.Vector (Vector, empty, cons, snoc)
import Grammar
import Symbol

grammarToList :: Grammar -> Vector Symbol
grammarToList = V.concatMap ( (Symbol Nothing `cons`) . (toSymbol `fmap`))

listToGrammar :: Vector Symbol -> Grammar
listToGrammar vec =
  case V.foldr go (empty, empty )  vec of
  (l,g) -> if null l then g else error "No ending Nothing"

go :: Symbol -> (Line, Grammar) -> (Line, Grammar)
go s (l, g) = go' (fromSymbol s)
  where
  go' Nothing = --Push the finished line
    ( empty
    , l `cons` g)
  go' (Just n) = -- Add the Node to the current line
    ( n `cons` l
    , g)
