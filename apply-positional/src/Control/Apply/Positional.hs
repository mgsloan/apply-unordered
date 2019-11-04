{-# LANGUAGE AllowAmbiguousTypes #-}
-- | FIXME
module Control.Apply.Positional
  ( applyN
  , ApplyAt
  ) where

import Data.Proxy (Proxy(..))
import Data.Type.Nat (Nat(..), FromGHC)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import qualified GHC.TypeLits as GHC

-- | NOTE: Only usable via TypeApplication
applyN
  :: forall (gn :: GHC.Nat) a f n. (n ~ FromGHC gn, ApplyAt n a f)
  => f -> a -> ApplyAtResult n a f
applyN = applyN' (Proxy :: Proxy gn)
{-# INLINE applyN #-}

applyN'
  :: forall (gn :: GHC.Nat) a f n. (n ~ FromGHC gn, ApplyAt n a f)
  => Proxy gn -> f -> a -> ApplyAtResult n a f
applyN' _ = applyAtImpl (Proxy :: Proxy n)
{-# INLINE applyN' #-}

-- | Typeclass used to implement 'applyN'. The first type argument is
-- the number of arguments to skip before doing the application.
class ApplyAt (n :: Nat) a f where
  type ApplyAtResult n a f
  applyAtImpl
    :: Proxy n
    -> f
    -> a
    -> ApplyAtResult n a f

instance a ~ b => ApplyAt Z a (b -> r) where
  type ApplyAtResult Z a (b -> r) = r
  applyAtImpl _ f x = f x
  {-# INLINE applyAtImpl #-}

instance ApplyAt n a r
      => ApplyAt (S n) a (b -> r) where
  type ApplyAtResult (S n) a (b -> r) = b -> ApplyAtResult n a r
  applyAtImpl _ f y = \x -> applyAtImpl (Proxy :: Proxy n) (f x) y
  {-# INLINE applyAtImpl #-}

-- TODO: better type error when running out of args
