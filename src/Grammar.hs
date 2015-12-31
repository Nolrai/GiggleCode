{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving, ExplicitForAll #-}

module Grammar
    ( Grammar (..)
    , Line (..)
    , Rules (..)
    , Node (..)
    , onNonterm
    ) where
import Data.Finite as F
import GHC.TypeLits
import qualified GHC.TypeLits as T
import Data.Vector as V
import qualified Prelude as P
import Prelude hiding (map)
import Control.Arrow (first)

instance KnownNat n => Read (Finite n) where
  readsPrec n s = P.map (first finite) $ readsPrec n s

data Node n t = Term t | Nonterm (Finite n)

deriving instance Eq t => Eq (Node n t)
deriving instance Ord t => Ord (Node n t)
deriving instance Show t => Show (Node n t)
deriving instance (KnownNat n, Read t) => Read (Node n t)

onNonterm :: (Finite n -> Finite m) -> Node n t -> Node m t
onNonterm _ (Term n) = Term n
onNonterm f (Nonterm n) = (Nonterm (f n))

data Line n t = Line (Node n t) (Node n t) (Vector (Node n t))

deriving instance Eq t => Eq (Line n t)
deriving instance Ord t => Ord (Line n t)
deriving instance Show t => Show (Line n t)
deriving instance (KnownNat n, Read t) => Read (Line n t)

data Rules (n :: Nat) t where
  Nil :: forall t. Rules 0 t
  Rule :: forall n t. Line n t -> Rules n t -> Rules (1 + n) t

data Grammar n t where
  Grammar :: forall n t. Line (1 + n) t -> Rules n t -> Grammar n t
