module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import qualified Data.Vector as V
import Data.Vector (Vector, empty, cons, snoc)
import Grammar
import Symbol as S
--import Debug.Trace

grammarToList :: Grammar -> Vector Symbol
grammarToList = V.concatMap ((endline `cons`) . (toSymbol `fmap`))

listToGrammar :: Vector Symbol -> Grammar
listToGrammar vec = go vec empty

go :: Vector Symbol -> Grammar -> Grammar
go vec so_far =
  if V.null vec
    then so_far
    else
      let (line, rest) = breakAtEndline vec
      in go (V.drop 1 rest) (so_far `snoc` line)
