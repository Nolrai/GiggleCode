{-# LANGUAGE
   FlexibleContexts
 , PolyKinds
 , TypeSynonymInstances
 , ConstraintKinds
 , TypeOperators
 , ExistentialQuantification
 , FlexibleInstances
 , MultiParamTypeClasses
#-}

module Utils where
import Data.Typeable (cast)

data Stub = Stub deriving (Read, Show, Enum, Ord, Eq)

class Stubable t where
  stub :: t
