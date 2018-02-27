{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs  #-}

module T2893b where

import Data.Coerce

newtype Wrap m a = Wrap (m a)

class Monad' m where
  join' :: m (m a) -> m a

-- instance (forall a. Coercible (m (m a)) (m (Wrap m a)), Monad' m) => Monad' (Wrap m) where
instance (forall p q. Coercible p q => Coercible (m p) (m q), Monad' m) => Monad' (Wrap m) where
  join' :: forall a. Wrap m (Wrap m a) -> Wrap m a
  join' = coerce @(m (m a) -> m a)
                 @(Wrap m (Wrap m a) -> Wrap m a)
                 join'

