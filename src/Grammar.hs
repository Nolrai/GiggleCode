module Grammar
    ( Grammar (..)
    , Line
    , NonTerm
    , Node (..)
    , Term
    ) where
import Data.Vector (Vector)

type Term = Char
type NonTerm = Int

data Node = TermNode Term | NonTermNode NonTerm
  deriving (Read, Show, Eq, Ord)

data Line = Line (Vector Node

data Grammar = Grammar Line (Vector Line)
  deriving (Read, Show, Eq, Ord)
