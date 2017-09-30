{-# LANGUAGE FlexibleInstances, FlexibleContexts, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module GrammarToListSpec
  ( spec
  ) where

import GrammarToList
  ( grammarToList
  , listToGrammar
  )

import Grammar (Grammar, Node(..))
import Symbol (UnsafeToNodeEndline, TailVEmptyVector, endline, toSymbol)

import TestUtils
import SymbolSpec (Ends, toValid, fromValid)

import Control.Monad.Exception

gltest :: Monad m => Grammar -> m Ends
lgtest 
  :: ( Throws UnsafeToNodeEndline l
     , Throws TailVEmptyVector l
     ) => Ends -> EM l Grammar
gltest = (fromValid <$>) . grammarToList
lgtest = listToGrammar . toValid

spec :: Spec
spec =
  describe "debug" 
  	$ gltest (Grammar [NonTermNode 0] []) 
	`shouldBe`
	fromValid [toSymbol (NonTermNode 0), endline]
  areInverses
    ("grammarToList", gltest)
    ("listToGrammar", lgtest)
