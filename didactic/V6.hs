{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module V6 where

import V5

import Data.Proxy (Proxy(..))

-- | A data-kind for 'HasArg' to return.
data HasArgResult
  = ArgPresent    -- ^ Indicates that the type is a function type.
  | NoArg         -- ^ Indicates that the type is not a function type.

-- | Checks whether the specified type is a function type ( @ -> @ ).
type family HasArg f :: HasArgResult where
  HasArg (_ -> _) = 'ArgPresent
  HasArg _ = 'NoArg

-- | Typeclass used to implement 'reorderArgs'. The first type
-- argument is used to select instances, and should always be @HasArg
-- f@.
class ReorderArgs (fHasArg :: HasArgResult) f g where
  reorderArgsImpl :: Proxy fHasArg -> f -> g

instance r1 ~ r2 => ReorderArgs 'NoArg r1 r2 where
  reorderArgsImpl _ x = x

instance
    ( HasAMatch a (b -> y) (b -> y)
    , matches ~ MatchFirstArg a (b -> y)
    , result ~ ApplyByTypeResult matches a (b -> y)
    , ApplyByType matches a (b -> y)
    , ReorderArgs (HasArg result) result x
    ) => ReorderArgs 'ArgPresent (b -> y) (a -> x) where
  reorderArgsImpl _ f x =
    reorderArgsImpl (Proxy :: Proxy (HasArg result)) $
    f ? x

reorderArgs :: forall f g. ReorderArgs (HasArg f) f g => f -> g
reorderArgs = reorderArgsImpl (Proxy :: Proxy (HasArg f))
