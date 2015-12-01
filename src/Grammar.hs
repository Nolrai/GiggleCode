module Grammarr
    ( Grammar
    , Line
    , NonTermHunk (..)
    , Term (..)
    ) where
import qualified Data.Text as T
import Data.Vector

newtype Term = Term {toChar :: Char}
  deriving (Read, Show, Eq, Ord)
newtype NonTermHunk = NonTerm {name :: Int}
  deriving (Read, Show, Eq, Ord)

type Line = [Either NonTermHunk Term]

type Grammar = Vector Line
