module Symbol
    ( Symbol (..)
    , toSymbol
    ) where
import Grammar (Node)

-- The Nothing value is used to seperate lines
newtype Symbol = Symbol {fromSymbol :: Maybe  Node}
  deriving (Eq, Ord)

toSymbol :: Node -> Symbol
toSymbol = Symbol . Just
