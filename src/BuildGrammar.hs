
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

{- step
 - stepNumber the step this is, counting from the inside out.
 - result : If Left then there are no repeated pairs left to encode, so shortcircut.
 -          If Right, then build a multi set of pairs, and encode the one that occurse the most.
 - -}

step :: Int -> Either (Rules Word8, [Node Word8]) -> Either (Rules Word8, [Node Word8])
step _ result@(Left _) = result
step stepNumber (Right input@(rules, list)) = if maxOccurs > 1 then Right (newRules, newList) else Left input
  where
  (multiSet, newList, _) = foldl' microStep (mempty, mempty, Nothing) list
  (newLine, maxOccurs) = L.maximumBy (compare `on` snd) $ M.toOccursList ms
  newRules = Rules newLine rules
  microStep :: (MultiSet (Word8, Word8), [Node Word8], Maybe Word8) -> Node Word8 -> (MultiSet (Word8, Word8), [Node Word8], Maybe Word8)
  microStep (multiSet', newList', Nothing) b = (multiSet', newList', Just b)
  microStep (multiSet', newList', Just b) a =
    let ab' =
        if (a,b) == newLine then NonTerminal stepNumber in --uses lazyness, make sure that newLine doesn't depend on newList
    ( insert (a,b) multiSet
    , ab' : tail newList'
    , Just a --Can't depend on newLine or we get infinite loop.
    )

buildGrammar :: Int -> ByteString -> Grammar Word8
buildGrammar maxSteps raw =
  uncurry Grammar . second toLine . either id id $ foldr step (Rules [], map Term toList raw) [maxSteps, maxSteps-1 .. 0]

--Recursively replace NonTerms by their entry in the dictionary
inflateGrammar :: Grammar Word8 -> ByteString
inflateGrammar (Grammar (Rules rules) line) = B.fromList $ recurse lookup (fromLine line)
  where
  vector = V.fromList rules
  lookup :: Node Word8 -> Either Word8 [Node Word8]
  lookup (Term word) = word
  lookup (NonTerm n) = fromLine $ vector V.! n

recurse :: (b -> Either a [b]) -> [b] -> [a]
recurse f = concatMap (either pure (recurse f))
