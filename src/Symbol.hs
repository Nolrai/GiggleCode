module Symbol
    ( Symbol (..)
    , toSymbol
    ) where
import Grammar (Node(..), Term(..), NonTerm(..))

-- The Nothing value is used to seperate lines
newtype Symbol = Symbol {fromSymbol :: Maybe  Node}
  deriving (Eq, Ord)

toSymbol :: Node -> Symbol
toSymbol = Symbol . Just

instance Show Symbol where
  show (Symbol Nothing) = "\\N"
  show (Symbol (Just (Node (Right (Term c))))) = show c
  show (Symbol (Just (Node (Left (NonTerm n))))) = "\\" ++ show n
