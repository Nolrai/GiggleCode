{-# LANGUAGE FlexibleContexts #-}
module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import qualified Data.Vector as V
import Data.Vector (Vector, empty, cons, snoc)
import Grammar
import Symbol as S
import Control.Monad.Exception
--import Debug.Trace

grammarToList :: Monad m => Grammar -> m (Vector Symbol)
grammarToList (Grammar main rules)
  = return $ V.concatMap ((endline `cons`) . (toSymbol <$>)) (V.cons main rules)

listToGrammar ::
  ( Throws S.UnsafeToNodeEndline l
  , Throws S.TailVEmptyVector l
  )
  => Vector Symbol
  -> EM l Grammar
listToGrammar vec =
  do
  (main, rest) <- breakAtEndline vec
  rules <- go rest empty
  return (Grammar main rules)

go ::
  ( Throws S.UnsafeToNodeEndline l
  , Throws S.TailVEmptyVector l
  )
  => Vector Symbol
  -> Vector Line
  -> EM l (Vector Line)
go vec so_far =
  if V.null vec
    then return so_far
    else
      do
      (line, rest) <- breakAtEndline vec
      go rest (so_far `snoc` line)