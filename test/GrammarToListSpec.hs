{-# LANGUAGE FlexibleInstances, FlexibleContexts, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module GrammarToListSpec
  ( spec
  ) where

import GrammarToList
  ( grammarToList
  , listToGrammar
  )

import Grammar (Grammar)
import Symbol (endline, Symbol)
import Symbol as S

import TestUtils
import SymbolSpec ()

import Control.Monad.Exception
import Data.Vector (init, Vector, snoc, last)
import Test.QuickCheck
import Prelude hiding (init, last)

gltest :: Monad m => Grammar -> m Ends
lgtest 
  :: ( Throws S.UnsafeToNodeEndline l
     , Throws S.TailVEmptyVector l
     ) => Ends -> EM l Grammar
gltest = (fromValid <$>) . grammarToList
lgtest = istToGrammar . toValid

spec :: Spec
spec =
  do
  ("fromValid", pure . fromValid) 
    `isInverseOf`
    ("fromEnds", pure . toValid)
  areInverses
    ("grammarToList", gltest)
    ("listToGrammar", lgtest)
