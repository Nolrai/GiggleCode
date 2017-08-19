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


newtype Ends = End {raw :: Vector Symbol}
  deriving (Arbitrary, Eq)

fromValid :: Vector Symbol -> Ends
fromValid v = 
  if last v == endline
    then End (init v)
    else error $ "Last Symbol is " ++ show (last v) ++ "not endline"
toValid :: Ends -> Vector Symbol
toValid End {raw} = raw `snoc` endline

instance Show Ends where
  show = show . toValid

gltest :: Monad m => Grammar -> m Ends
lgtest 
  :: ( Throws S.UnsafeToNodeEndline l
     , Throws S.TailVEmptyVector l
     ) => Ends -> EM l Grammar
gltest = (fromValid <$>) . grammarToList
lgtest = listToGrammar . toValid

spec :: Spec
spec =
  do
  ("fromValid", pure . fromValid) 
    `isInverseOf`
    ("fromEnds", pure . toValid)
  areInverses
    ("grammarToList", gltest)
    ("listToGrammar", lgtest)
