module PadicEncode
    ( encode
    , decode
    , module B
    ) where

import Data.Vector
import qualified Data.ByteString.Lazy as B

encode :: Ord a => Vector a -> B.ByteString
decode :: Ord a => B.ByteString -> Vector a

encode = undefined
decode = undefined

