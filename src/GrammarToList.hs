module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import qualified Data.Vector as V
import Data.Vector (Vector, empty, cons, snoc)
import Grammar
import Symbol
import Debug.Trace

grammarToList :: Grammar -> Vector Symbol
grammarToList = V.concatMap ( (Symbol Nothing `cons`) . (toSymbol `fmap`))

listToGrammar :: Vector Symbol -> Grammar
listToGrammar vec =
  case V.foldr goT (empty, empty) vec of
  (l,g) -> if null l then g else snd $ goT (Symbol Nothing) (l,g)

goT x = trace ("goT :" ++ show x) (go x)

go :: Symbol -> (Line, Grammar) -> (Line, Grammar)
go s (l, g) = goT' (fromSymbol s)
  where
  goT' x = trace ("go' :" ++ show x) (go' x)
  go' Nothing = --Push the finished line
      ( empty
      , l `cons` g)

  go' (Just n) = -- Add the Node to the current line
      ( n `cons` l
      , g)
