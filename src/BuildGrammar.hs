
module BuildGrammar
    ( buildGrammar
    , inflateGrammar
    ) where
import qualified ByteString as B
import ByteString (ByteString)
import qualified Data.Vector as V
import Grammar
import Data.Word
import Data.MultiSet as M
import Data.List as L

{-
toVector
  :: B.Bytestring
  -> (Rules 1 Word8, V.Vector (Glif 0 Word8))
toVector str = threeToTwo (B.fold step (mempty, mempty, Nothing) str)
where
  threeToTwo (ms, vec, last) = (ms, vec)
  step c (ms, vec, next) =
    ( maybe ms (\next' -> insert (c, next') ms) next
    , cons c vec
    , c
    )

step :: KnownNat n => Rules (1 + n) Word8 -> V.Vector (Glif n Word8) -> (Rules (2 + n) Word8, V.Vector (Glif (1+n) Word8))
step rules vec = (newRules, newVec)
  where
  (ms, newVec) = V.fold microStep (mepmpty, mempty, Nothing)
  newLine = L.maximumBy (compare `on` snd) $ M.toOccursList ms
  newRules = Rules newLine rules

Type Iter = 100

class KnownNumber n => Iter n where
  inter :: Proxy n -> (forall m. A m -> A (1 + m)) -> A 0 -> A n

instance KnownNumber 0 => Iter 0 where
  iter _ _ a = a

instance Iter n => Iter (1 + n) where
  iter Proxy f a = f (iter Proxy  a)

type IterMax = 10
-}

buildGrammar :: ByteString -> Grammar Word8
buildGrammar raw =
  --uncurry Grammar $ iter (Proxy :: Proxy Max) (uncurry step) $ toVector raw
  undefined

inflateGrammar :: Grammar Word8 -> ByteString
inflateGrammar = undefined
