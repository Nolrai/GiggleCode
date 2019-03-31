module Tokenize where

import Data.Attoparsec.Text as P
import Control.Arrow as A
import Data.Char as C

token = takeWhile1 . token' <<= char
  where
  token' c
  | isAlphaNum c = takeWhile $ isAlphaNum ||| isMark
  | isSeperator c = takeWhile $ isSeperator
  | isPunctutation c = takeWhile $ isPunctutation ||| isMark
  | isSymbol c = takeWhile $ isSymbol ||| isMark

toTokens =