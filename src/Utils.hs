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
import Control.Monad.Exception.IO
import Data.Typeable (cast)

class Stubable t where
  stub :: t

instance (Monad m, Throws Stub l) => Stubable (EMT l m a) where
  stub = throw Stub

instance Stubable s => Stubable (b -> s) where
  stub = const stub

data Stub = Stub deriving (Show)

instance Exception Stub where
  toException = giggleCodeExceptionToException
  fromException = giggleCodeExceptionFromException

instance Throws Stub (Caught GiggleCodeException l) where

data GiggleCodeException = forall e . Exception e => GiggleCodeException e

instance Show GiggleCodeException where
    show (GiggleCodeException e) = "GiggleCode:" ++ show e

instance Exception GiggleCodeException where

instance Throws GiggleCodeException (Caught GiggleCodeException l) where

giggleCodeExceptionToException :: Exception e => e -> SomeException
giggleCodeExceptionToException = toException . GiggleCodeException

giggleCodeExceptionFromException :: Exception e => SomeException -> Maybe e
giggleCodeExceptionFromException x =
    do
    GiggleCodeException a <- fromException x
    cast a

type EMG = EM AnyException

