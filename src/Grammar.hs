{-# LANGUAGE StandaloneDeriving, GADTSyntax, DeriveGeneric #-}

module Grammar
    ( Grammar (..)
    , Line (..)
    , Rules (..)
    , Node (..)
    , verifyGrammar
    , verifyRules
    , verifyLine
    , verifyNode
    ) where
import qualified Prelude as P
import Prelude hiding (map)
import Control.Arrow (first)
import Numeric.Natural
import Control.Monad
import GHC.Generics

-- A node is either term, or reference to a lower line
data Node t = Term t | Nonterm Natural
  deriving (Eq, Ord, Show, Read, Generic)

--A line is at least a pair of nodes.
data Line t = Line (Node t) (Node t)  [(Node t)]
  deriving (Eq, Ord, Show, Read, Generic)

lineToList (Line a b rest) = a : b : rest


-- A dictionary, or "Rules" is a list of lines.
newtype Rules t = Rules [Line t]
  deriving (Eq, Ord, Show, Read, Generic)

-- A complete Grammar includes the "start" line as well.
--   Usually that will be much larger then the resT.
data Grammar t where
  Grammar :: Line t -> Rules t -> Grammar t
  deriving (Eq, Ord, Show, Read, Generic)

-- Verify that all the nodes in a Grammar refer to lines they know about.
verifyGrammar :: Grammar t -> Maybe ()
verifyGrammar (Grammar l r) =
  do
  n <- verifyRules r
  verifyLine n l

-- The heart of verifyGrammar,
-- (returns the number of rule lines,
--   so that verifyGrammar knows what the "start" line can referto.)
verifyRules (Rules l) =
  do
  u <- zipWithM (\ i line -> verifyLine i line) [0..] (reverse l)
  return (length u)

-- Verify that none of the nodes in this line refer to a line above n.
verifyLine n l = P.mapM_ (verifyNode n) (lineToList l)

--"verifyNode n node" make sure "node" only referes to lines below, (n or less).
verifyNode _ (Term t) = return ()
verifyNode n (Nonterm m) = guard (n > fromIntegral m)
