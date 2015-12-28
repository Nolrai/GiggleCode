{-# LANGUAGE StandaloneDeriving, GADTs, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Glif
    ( Glif (..)
    , toGlif
    ) where
import Grammar (Node (..), onNonterm)
import GHC.TypeLits
import Data.Finite

-- The Nothing value is used to seperate lines
newtype Glif n (t :: *) = Glif {fromGlif :: Maybe (Node n t) }

deriving instance (KnownNat m, Eq t) => Eq (Glif m t)
deriving instance (KnownNat m, Ord t) => Ord (Glif m t)

toGlif :: (m <= n) => Node m t -> Glif n t
toGlif n = Glif (Just (onNonterm weakenN n))
