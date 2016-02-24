
module BuildGrammar
    ( buildGrammar
    , inflateGrammar
    ) where
import qualified ByteString as B
import ByteString (ByteString)
import Grammar
import Data.Word
import Data.MultiSet as M
import Data.List as L
import qualified Data.Vector as V
import Data.Function (on)
import Control.Arrow (second)
import Control.Monad (foldM)

{- step
 - stepNumber the step this is, counting from the inside out.
 - result : If Left then there are no repeated pairs left to encode, so we are done.
 -          If Right, then build a multi set of pairs, and encode the one that occurse the most.
 - -}

type ShortCircuit a = Either a a
type Pair a = (a, a)

runShortCircuit :: ShortCircuit a -> a
runShortCircuit = either id id

done :: ShortCircuit a
done a = Left a

step :: Int -> (Rules Word8, [Node Word8]) -> ShortCircuit (Rules Word8, [Node Word8])
step stepNumber input@(rules, list) = if maxOccurs > 1 then return (newRules, newList) else done input
  where
  multiSet :: MultiSet (Pair (Node Word8))
  newList :: [Node Word8]
  (multiSet, newList, _) = foldl' microStep (mempty, mempty, Nothing) list
  newLine :: Pair (Node Word8)
  (newLine, maxOccurs) = L.maximumBy (compare `on` snd) $ M.toOccurList multiSet
  newRules = Rules newLine rules
  microStep :: (MultiSet (Pair (Node Word8)), [Node Word8], Maybe Word8) -> Node Word8 -> (MultiSet (Pair (Node Word8)), [Node Word8], Maybe Word8)
  microStep (multiSet', newList', Nothing) b = (multiSet', newList', Just b)
  microStep (multiSet', newList', Just b) a =
    let ab' =
              if (a,b) == newLine
              then [Nonterm stepNumber]
              else [a,b] in --uses lazyness, make sure that newLine doesn't depend on newList
    (M.insert (a,b) multiSet
    , ab' ++ tail newList'
    , Just a --Can't depend on newLine or we get infinite loop.
    )

runShortCircuitSteps :: (Int -> a -> ShortCircuit a) -> Int -> a -> a
runShortCircuitSteps singleStep maxSteps start
  = runShortCircuit $ foldM singleStep start [0 .. maxSteps]

buildGrammar :: Int -> ByteString -> Grammar Word8
buildGrammar maxSize raw =
  uncurry Grammar . second toLine $ runShortCircuitSteps step maxSize (Rules [], Term `fmap` B.unpack raw)

--Recursively replace NonTerms by their entry in the dictionary
inflateGrammar :: Grammar Word8 -> ByteString
inflateGrammar (Grammar (Rules rules) line) = B.pack . recurse lookup $ (fromLine line)
  where
  vector = V.fromList rules
  lookup :: Node Word8 -> Either Word8 [Node Word8]
  lookup (Term word) = word
  lookup (Nonterm n) = fromLine $ vector V.! (fromIntegral n)

recurse :: (a -> Either b [a]) -> [a] -> [b]
recurse f = go where
  go :: [a] -> [b]
  go = concatMap g
  g :: a -> [b]
  g a = either pure go (f a)

