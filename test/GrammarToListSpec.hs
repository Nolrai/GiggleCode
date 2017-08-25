{-# LANGUAGE FlexibleInstances, FlexibleContexts, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module GrammarToListSpec
  ( spec
  ) where

import GrammarToList
  ( grammarToList
  , listToGrammar
  )

import Grammar (Grammar)
import Symbol (UnsafeToNodeEndline, TailVEmptyVector)

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
  do
  ("fromValid", pure . fromValid) 
    `isInverseOf`
    ("toValid", pure . toValid)
  areInverses
    ("grammarToList", gltest)
    ("listToGrammar", lgtest)