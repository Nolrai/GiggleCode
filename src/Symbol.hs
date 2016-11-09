module Symbol
    ( Symbol (..)
    , toSymbol
    , isNode
    , unsafeToNode
    , breakAtEndline
    , unBreakAtEndline
    , endline
    ) where

import Grammar (Node(..), Term(..), NonTerm(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromJust, isJust)
import Control.Arrow (first)

-- The Nothing value is used to seperate lines
newtype Symbol = Symbol {fromSymbol :: Maybe Node}
  deriving (Eq, Ord)

endline :: Symbol
endline = Symbol Nothing

toSymbol :: Node -> Symbol
toSymbol = Symbol . Just

isNode :: Symbol -> Bool
unsafeToNode :: Symbol -> Node
isNode = isJust . fromSymbol
unsafeToNode = fromJust . fromSymbol

-- breakAtNothing takes a vector of Symbols and breaks off the prefix
--   of 'Just x's and converts them to a vector of nodes.
breakAtEndline :: Vector Symbol -> (Vector Node, Vector Symbol)
breakAtEndline = first (fmap unsafeToNode) . V.break (not . isNode)

-- Testing only.
unBreakAtEndline :: (Vector Node, Vector Symbol) -> Vector Symbol
unBreakAtEndline (line, rest) = (fmap toSymbol line) `mappend` (endline `V.cons` rest)

instance Show Symbol where
  show (Symbol Nothing) = "\\N"
  show (Symbol (Just (Node (Right (Term c))))) = show c
  show (Symbol (Just (Node (Left (NonTerm n))))) = "\\" ++ show n
