{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GrammarToList
    ( grammarToList
    , listToGrammar
    ) where
import Grammar
import qualified Data.List as L
import Data.Maybe (fromMaybe, maybe)

grammarToList :: forall a. Grammar a -> [Maybe (Node a)]
grammarToList (Grammar l r) =
  foldr sepByNothing [] ( l' :  r')
  where
  sepByNothing :: [Node a] -> [Maybe (Node a)] -> [Maybe (Node a)]
  sepByNothing x' acc =
    let x = map Just x' in
      case acc of
      [] -> x
      xs -> x ++ (Nothing : xs)
  l' :: [Node a]
  l' = lineToList l
  r' :: [[Node a]]
  r' = rulesToList r

rulesToList :: Rules a -> [[Node a]]
rulesToList (Rules a) = map lineToList a

listToRules :: Show a => [[Node a]] -> Rules a
listToRules = Rules . map listToLine

lineToList :: Line a -> [Node a]
lineToList (Line a b rest) = a : b : rest

listToGrammar :: Show a => [Maybe (Node a)] -> Grammar a
listToGrammar l =
  case splitNothings l of
  (line, rules) -> Grammar (listToLine line) (listToRules rules)

splitNothings :: [Maybe a] -> ([a], [[a]])
splitNothings =
  foldr go ([],[])
  where
  go n (l, r) =
    case n of
    Nothing -> --Add finished line to rules
      ([], l : r)
    Just a -> --Add to current line
      (a : l, r)

listToLine :: Show a => [Node a] -> Line a
listToLine (a:b:rest) = Line a b rest
listToLine other = error $ "toLine called on (" ++  show other ++  ")"
