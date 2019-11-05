{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | FIXME
module Control.Apply.Positional
  ( applyN
  , ApplyAt
  , CheckArity
  ) where

import GHC.Exts (Constraint)
import Data.Proxy (Proxy(..))
import Data.Type.Nat (Nat(..), FromGHC)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import qualified GHC.TypeLits as GHC

-- | FIXME: document
--
-- NOTE: Only usable via TypeApplication
applyN
  :: forall (gn :: GHC.Nat) a f n.
     ( n ~ FromGHC gn
     , CheckArity gn f n f
     , ApplyAt n a f
     )
  => f -> a -> ApplyAtResult n a f
applyN = applyN' (Proxy :: Proxy gn)
{-# INLINE applyN #-}

-- | FIXME: document
applyN'
  :: forall (gn :: GHC.Nat) a f n.
     ( n ~ FromGHC gn
     , CheckArity gn f n f
     , ApplyAt n a f
     )
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

type family CheckArity (n0 :: GHC.Nat) f0 (n :: Nat) f :: Constraint where
  CheckArity n0 f0 (S n) (_ -> r) = CheckArity n0 f0 n r
  CheckArity n0 f0 Z (_ -> r) = ()
  CheckArity n0 f0 (S n) r = TypeError (TooFewParametersMsg n0 f0)
  CheckArity n0 f0 Z r = TypeError (TooFewParametersMsg n0 f0)

type TooFewParametersMsg (n :: GHC.Nat) f =
  'Text "Cannot apply argument index " :<>: 'ShowType n :<>:
  'Text " of a function of type" :$$:
  'Text "  " :<>: 'ShowType f :$$:
  'Text "because it has too few arguments."
