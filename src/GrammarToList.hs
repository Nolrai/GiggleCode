{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import qualified Data.Vector as V
import Data.Vector (Vector, empty, cons, snoc)
import Grammar
import Glif
import GHC.TypeLits
import Data.Finite (strengthen, weaken)
import Data.Maybe (fromMaybe, maybe)

data Proxy n

grammarToList :: Show a => Grammar n a -> Vector (Glif (1 + n) a)
grammarToList (Grammar l r) =
  V.concatMap ( (Glif Nothing `cons`) . (toGlif `fmap`)) ( l' `V.cons`  r')
  where
  l' = lineToList l
  r' = rulesToList r

rulesToList :: Rules n a -> Vector (Vector (Node n a))
rulesToList Nil = empty
rulesToList (Rule l r) = lineToList l : V.map (onNonterm weaken) r

lineToList :: Line n a -> Vector (Node n a)
lineToList (Line a b rest) = a `V.cons` (b `V.cons` rest)

listToGrammar :: Show a => Vector (Glif n a) -> Grammar n a
listToGrammar vec =
  case V.foldr go (mempty, mempty)  vec of
  ([], l:g) -> toGrammar' l g
  (l , g  ) -> toGrammar' l g

toGrammar' :: Show a => [Node n a] -> [[Node n a]] -> Grammar n t
toGrammar' l r = Grammar (toLine l) (toRules . strengthen' r)

strengthen' :: Show a => [[Node (1 + n) a]] -> [[Node n a]]
strengthen' = map (map strengthenNode)

strengthenNode :: Show a => Node (1 + n) a -> Node n a
strengthenNode =
  onNonterm (fromMaybe (error "failed strengthen") strengthen)

toRules :: (Show a, KnownNat n) => [[Node n a]] -> Rules (1 + n) a
toRules l = let p = (error "" :: Proxy n) in
  case (l, natVal p) of
    ([x] , 0) -> Rule (toLine x) Nil
    (x:xs, n) -> Rule (toLine x) (toRules $ strengthen' xs)
    (_   , n) ->
      error $ "invalid list into to toRules,"
        ++ " n = "  ++ show n
        ++ ", l = " ++ show l

toLine :: Show a => [Node n a] -> Line n a
toLine (a:b:rest) = Line a b (V.fromList rest)
toLine other = error $ "toLine called on (" ++ show other ++ ")"

go ::
  forall n a. Show a =>
  Glif n a ->
  ([Node n a], [[Node n a]]) ->
  ([Node n a], [[Node n a]])
go s (l, g) = go' (fromGlif s)
  where
  go' :: Maybe (Node n a) -> ([Node n a], [[Node n a]])
  go' Nothing = --Push the finished line
    ( mempty
    , l : g)
  go' (Just n) = -- Add the Node to the current line
    ( n : l
    , g)
