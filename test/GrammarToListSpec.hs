{-# LANGUAGE FlexibleInstances, FlexibleContexts, NamedFieldPuns #-}
module GrammarToListSpec
  ( spec
  ) where

import GrammarToList
  ( grammarToList
  , listToGrammar
  )

import Grammar (Grammar(..), Node(..))
import Symbol
  (UnsafeToNodeEndline, TailVEmptyVector, endline, toSymbol)
import TestUtils
import SymbolSpec (Ends, toValid, fromValid)
import Control.Monad.Exception
import Data.Vector (fromList)

gltest :: Monad m => Grammar -> m Ends
lgtest 
  :: ( Throws UnsafeToNodeEndline l
     , Throws TailVEmptyVector l
     ) => Ends -> EM l Grammar
gltest g = fromValid <$> grammarToList g
lgtest = listToGrammar . toValid

spec :: Spec
spec = part1 >> part2
  where
  part1 :: Spec
  part1 =
    it "takes an empty grammar to an almost empty vector" . example 
    $ do 
      result <- gltest (Grammar (fromList [NonTermNode 0]) mempty)
      let expected = fromValid (fromList [toSymbol (NonTermNode 0), endline])
      result `shouldBe` expected
      
  part2 :: Spec
  part2 =
    areInverses
      ("grammarToList", gltest)
      ("listToGrammar", lgtest)
