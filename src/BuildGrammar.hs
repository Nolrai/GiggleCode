{-# LANGUAGE
   FlexibleContexts
 , FlexibleInstances
 , MultiParamTypeClasses
  #-}
module BuildGrammar
    ( buildGrammar
    , inflateGrammar
    ) where
import qualified Data.Text.Lazy as T
import Grammar (Grammar(..), Node(..), Term, NonTerm, Line)
import Data.Vector
import Control.Monad.Exception
import qualified Data.Vector as V
import Utils

buildGrammar :: (Throws Stub l) => T.Text -> EM l Grammar
buildGrammar = stub

inflateGrammar :: (Throws InvalidNonTerm l) => Grammar -> EM l T.Text
inflateGrammar (Grammar main rules) =
  lineToText <$> lookupNode rules main

lookupNode :: (Throws InvalidNonTerm l) => Vector Line -> Line -> EM l (Vector Term)
lookupNode rules main = V.sequence (go =<< main)
  where
  go :: (Throws InvalidNonTerm l) => Node -> Vector (EM l Term)
  go (NonTermNode n)
    | n < V.length rules = go =<< (rules ! n)
    | otherwise = pure (throw $ InvalidNonTerm n)
  go (TermNode n) = pure (pure n)

lineToText :: Vector Term -> T.Text
lineToText = T.pack . V.toList

data InvalidNonTerm = InvalidNonTerm NonTerm
  deriving (Show)

instance Exception InvalidNonTerm where
  toException = giggleCodeExceptionToException
  fromException = giggleCodeExceptionFromException

instance Throws InvalidNonTerm (Caught GiggleCodeException l)
