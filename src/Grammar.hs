module Grammar
    ( Grammar (..)
    , Word5
    , Node (..)
    , toNode
    , fromNode
    , isTerm
    , isNonTerm
    ) where
import Data.Text.Lazy as T
import Data.ShortWord (Word5)
import Data.Vector
import Data.Char(chr, ord)

-- "In addition, there is a contiguous range of another 32 noncharacter code 
--   points in the BMP: U+FDD0..U+FDEF." 
--   - from section 23.7 of the Unicode Standard 
-- We use these to code NonTerm symbols in the built grammar, 
--   there are another 32 noncharacter codes, but they are non contiguous.
nonTermStart = '\xFDD0'
nonTermEnd = '\xFDEF'

toNode :: Char -> Node
toNode t = 
  if isNonTerm t
    then NonTerm (toEnum $ ord t - ord nonTermStart) 
    else Term t

fromNode :: Node -> Char
fromNode (Term t) = t
fromNode (NonTerm x) = chr $ ord nonTermStart + fromEnum x

asNode :: (Node -> Node) -> Char -> Char
asNode f = fromNode . f . toNode

isTerm :: Char -> Bool
isNonTerm :: Char -> Bool
isTerm t = t < nonTermStart || t > nonTermEnd
isNonTerm t = t >= nonTermStart && t <= nonTermEnd

data Node = Term Char | NonTerm Word5

data Grammar = Grammar Text (Vector Text)
  deriving (Read, Show, Eq, Ord)
