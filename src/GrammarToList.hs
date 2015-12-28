module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import qualified Data.Vector as V
import Data.Vector (Vector, empty, cons, snoc)
import Grammar
import Glif

grammarToList :: Grammar a -> Vector (Glif n a)
grammarToList = V.concatMap ( (Glif Nothing `cons`) . (toGlif `fmap`))

listToGrammar :: Vector (Glif n a) -> Grammar a
listToGrammar vec =
  case V.foldr go (empty, empty )  vec of
  (l,g) -> if null l then g else error "No ending Nothing"

go :: Glif n a -> (Line n a, Grammar a) -> (Line n a, Grammar a)
go s (l, g) = go' (fromGlif s)
  where
  go' Nothing = --Push the finished line
    ( empty
    , l `cons` g)
  go' (Just n) = -- Add the Node to the current line
    ( n `cons` l
    , g)
