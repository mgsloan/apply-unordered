{-# LANGUAGE UndecidableInstances #-}

module Control.Apply.Unordered where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import qualified GHC.TypeLits as GHC
import Data.Proxy
import Data.Type.Nat (Nat(..), FromGHC)

import Control.Apply.Positional

infixl 1 ?

(?)
  :: forall a f gn n.
     ( gn ~ BestParamIxImpl a f
     , n ~ FromGHC gn
     , CheckArity gn f n f
     , ApplyAt n a f
     )
  => f -> a -> ApplyAtResult n a f
(?) = applyN' (Proxy :: Proxy gn)

type family BestParamIxImpl (a :: *) (f :: *) :: GHC.Nat where

{- TODO: Might be handy for other users of this to

type BestParamIx a f = RaiseError (BestParamIxImpl a f)

type family RaiseError (a :: Either ErrorMessage k) :: k where
  RaiseError (Left msg) = TypeError msg
  RaiseError (Right x) = x

type family BestParamIxImpl (a :: *) (f :: *) :: Either ErrorMessage GHC.Nat where
  BestParamIxImpl _ _ = TypeError PluginNotEnabledErrorMsg
-}


{- FIXME: use the stuckness hack

  BestParamIxImpl _ _ = TypeError PluginNotEnabledErrorMsg

type PluginNotEnabledErrorMsg =
  'Text "The unordered apply plugin isn't enabled." :$$:
  'Text "  Fix: import and enable '-fplugin=Control.Apply.Unordered.Plugin'"
-}
