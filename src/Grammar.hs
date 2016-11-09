module Grammar
    ( Grammar
    , Line
    , NonTerm (..)
    , Node (..)
    , Term (..)
    ) where
import Data.Vector (Vector)

newtype Term = Term {toChar :: Char}
  deriving (Read, Show, Eq, Ord)
newtype NonTerm = NonTerm {name :: Int}
  deriving (Read, Show, Eq, Ord)
newtype Node = Node (Either NonTerm Term)
  deriving (Read, Show, Eq, Ord)

type Line = Vector Node

type Grammar = Vector Line
