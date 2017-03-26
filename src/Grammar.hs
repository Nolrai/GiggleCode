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

type Line = Vector Node

data Grammar = Grammar Line (Vector Line)
