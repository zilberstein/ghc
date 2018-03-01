{-# LANGUAGE QuantifiedConstraints, PolyKinds
           , StandaloneDeriving, RoleAnnotations
           , UndecidableInstances
           , GeneralizedNewtypeDeriving #-}

module T9123 where

import Data.Coerce

class MyMonad m where
  join :: m (m a) -> m a

data StateT s m a = StateT (s -> m (a, s))   -- could be a newtype, but that 
                                             -- doesn't change my argument

type role StateT nominal representational nominal   -- as inferred
instance Monad m => Monad (StateT s m) where

newtype IntStateT m a = IntStateT (StateT Int m a)
  deriving Monad

deriving instance (Applicative m, forall a. Coercible a b => Coercible (m a) (m b)) 
               => Applicative (IntStateT m)

deriving instance (Monad m, forall a. Coercible a b => Coercible (m a) (m b)) 
               => Monad (IntStateT m)
