{-# LANGUAGE FlexibleContexts #-}
module PadicEncode
    ( encode
    , decode
    ) where

import Data.Vector
import Control.Monad.Exception (EM, Throws)
import qualified B
import Utils (stub, Stub)

encode :: (Ord a, Throws Stub l) => Vector a -> EM l B.ByteString
decode :: (Ord a, Throws Stub l) => B.ByteString -> EM l (Vector a)

encode = stub
decode = stub

