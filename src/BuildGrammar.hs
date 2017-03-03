{-# LANGUAGE TypeOperators, ViewPatterns #-}
module BuildGrammar
    ( buildGrammar
    , inflateGrammar
    ) where
import qualified Data.Text.Lazy as T
import Grammar
import Control.Arrow ((+++))
import Data.Vector
import Control.Monad.Exception
import qualified Data.Vector as V

buildGrammar = undefined

inflateGrammar (uncons -> (main, rules)) =
  lineToText <$> V.mapM (lookupNode rules) main
inflateGrammar _ = throw EmptyGrammar

lookupNode rules = (\n -> rules !! n) +++ return

lineToText = T.pack . V.toList <$> V.mapM nodeToText

nodeToText (TermNode a) = return a
nodeToText (NonTermNode b) = throw UnreplacedNonTerm

data UnreplacedNonTerm = UnreplacedNonTerm
  deriving (Show)

instance Exception UnreplacedNonTerm

data EmptyGrammar = EmptyGrammar
  deriving (Show)

instance Exception EmptyGrammar

