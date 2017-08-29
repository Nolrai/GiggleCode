{-# LANGUAGE
   FlexibleContexts
 , FlexibleInstances
 , MultiParamTypeClasses
  #-}
module Symbol
    ( Symbol (..)
    , toSymbol
    , isNode
    , unsafeToNode
    , breakAtEndline
    , unBreakAtEndline
    , endline
    , UnsafeToNodeEndline (..)
    , TailVEmptyVector
    ) where

import Grammar (Node(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Control.Monad.Exception
import Data.Monoid ((<>))
import Utils

-- The Nothing value is used to seperate lines
newtype Symbol = Symbol (Maybe Node)
  deriving (Eq,Ord)

endline :: Symbol
endline = Symbol Nothing

toSymbol :: Node -> Symbol
toSymbol = Symbol . Just

isNode :: Symbol -> Bool
isNode (Symbol s) = isJust s

unsafeToNode ::
  (Throws UnsafeToNodeEndline l, Monad m)
  => Symbol -> EMT l m Node
unsafeToNode (Symbol (Just n)) = return n
unsafeToNode (Symbol Nothing ) = throw UnsafeToNodeEndline

data UnsafeToNodeEndline = UnsafeToNodeEndline
  deriving (Show)

instance Exception UnsafeToNodeEndline where
  toException = giggleCodeExceptionToException
  fromException = giggleCodeExceptionFromException

instance Throws UnsafeToNodeEndline (Caught GiggleCodeException l)

-- breakAtEndline takes a vector of Symbols and breaks off the prefix
--   of 'Just x's and converts them to a vector of nodes.
breakAtEndline ::
  ( Throws TailVEmptyVector l
  , Throws UnsafeToNodeEndline l
  , Monad m)
  => Vector Symbol
  -> EMT l m (Vector Node, Vector Symbol)
breakAtEndline v =
  do
  let (front, back) = (V.break (not . isNode)) v
  line <- V.mapM unsafeToNode front
  rest <- tailV back
  return (line, rest)

tailV ::
  ( Throws TailVEmptyVector l
  , Monad m)
  => Vector a
  -> EMT l m (Vector a)
tailV v = if V.null v then throw TailVEmptyVector else return (V.drop 1 v)

data TailVEmptyVector = TailVEmptyVector
  deriving (Show)

instance Exception TailVEmptyVector where
  toException = giggleCodeExceptionToException
  fromException = giggleCodeExceptionFromException

instance Throws TailVEmptyVector (Caught GiggleCodeException l)

instance Show Symbol where
  show (Symbol s) =
    case s of
      Nothing -> "\\N"
      Just (TermNode c) -> show c
      Just (NonTermNode n) -> "<" ++ show n ++ ">"

-- Testing only.
unBreakAtEndline :: (Vector Node, Vector Symbol) -> Vector Symbol
unBreakAtEndline (line, rest) = fmap toSymbol line <> (endline `V.cons` rest)

