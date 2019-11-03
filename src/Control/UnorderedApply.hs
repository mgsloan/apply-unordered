module Control.UnorderedApply
  (
    -- * Type-directed function application which uses first match
    applyByType
  , reorderArgs
  , (?)
    -- * Type-directed function application which requires unique match
  , applyByUniqueType
  , reorderUniqueArgs
  , (?!)
    -- * Machinery for type directed function application
  , MatchArgResult(..)
  , MatchFirstArg
  , HasAMatch
  , ApplyByType(ApplyByTypeResult)
    -- * Machinery for unique match function application
  , HasUniqueMatch
  , CheckAmbiguousMatch
    -- * Machinery for reordering arguments
  , HasArgResult(..)
  , HasArg
  , ReorderArgs
  , ReorderUniqueArgs
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))

applyByType, (?)
  :: forall matches a f.
     ( matches ~ HasAMatch a f f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
applyByType = applyByTypeImpl (Proxy :: Proxy matches)
{-# INLINE applyByType #-}

infixl 1 ?
(?) = applyByType
{-# INLINE (?) #-}

applyByUniqueType, (?!)
  :: forall matches a f.
     ( matches ~ HasUniqueMatch a f f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
applyByUniqueType = applyByTypeImpl (Proxy :: Proxy matches)
{-# INLINE applyByUniqueType #-}

infixl 1 ?!
(?!) = applyByUniqueType
{-# INLINE (?!) #-}

reorderArgs :: forall f g. ReorderArgs (HasArg f) f g => f -> g
reorderArgs = reorderArgsImpl (Proxy :: Proxy (HasArg f))

reorderUniqueArgs :: forall f g. ReorderUniqueArgs (HasArg f) f g => f -> g
reorderUniqueArgs = reorderUniqueArgsImpl (Proxy :: Proxy (HasArg f))

--------------------------------------------------------------------------------
-- Match machinery

data MatchArgResult
  = Matches
  | Doesn'tMatch
  | NoArgToMatch

type family MatchFirstArg a f :: MatchArgResult where
  MatchFirstArg a (a -> r) = 'Matches
  MatchFirstArg a (b -> r) = 'Doesn'tMatch
  MatchFirstArg _ _ = 'NoArgToMatch

type family HasAMatch a f f0 :: MatchArgResult where
  HasAMatch a (a -> r) f0 = MatchFirstArg a f0
  HasAMatch a (b -> r) f0 = HasAMatch a r f0
  HasAMatch a _ f0 = TypeError (NoMatchError a f0)

class ApplyByType (matches :: MatchArgResult) a f where
  type ApplyByTypeResult matches a f
  applyByTypeImpl
    :: Proxy matches
    -> f
    -> a
    -> ApplyByTypeResult matches a f

instance ApplyByType 'Matches a (a -> r) where
  type ApplyByTypeResult 'Matches a (a -> r) = r
  applyByTypeImpl _ f x = f x
  {-# INLINE applyByTypeImpl #-}

instance ApplyByType (MatchFirstArg a r) a r
      => ApplyByType 'Doesn'tMatch a (b -> r) where
  type ApplyByTypeResult 'Doesn'tMatch a (b -> r) =
    b -> ApplyByTypeResult (MatchFirstArg a r) a r
  applyByTypeImpl _ f y =
    \x -> applyByTypeImpl (Proxy :: Proxy (MatchFirstArg a r)) (f x) y
  {-# INLINE applyByTypeImpl #-}

instance TypeError (NoMatchForResultError a r)
      => ApplyByType 'NoArgToMatch a r where
  type ApplyByTypeResult 'NoArgToMatch a r =
    TypeError (NoMatchForResultError a r)
  applyByTypeImpl = error "impossible"

--------------------------------------------------------------------------------
-- Unique match machinery

type family HasUniqueMatch a f f0 :: MatchArgResult where
  HasUniqueMatch a (a -> r) f0 = CheckAmbiguousMatch a r f0
  HasUniqueMatch a (b -> r) f0 = HasUniqueMatch a r f0
  HasUniqueMatch a _ f0 = TypeError (NoMatchError a f0)

type family CheckAmbiguousMatch a f f0 :: MatchArgResult where
  CheckAmbiguousMatch a (a -> r) f0 = TypeError (AmbiguousMatchError a f0)
  CheckAmbiguousMatch a (b -> r) f0 = CheckAmbiguousMatch a r f0
  CheckAmbiguousMatch a _ f0 = MatchFirstArg a f0

--------------------------------------------------------------------------------
-- Argument reordering machinery

data HasArgResult = ArgPresent | NoArg

type family HasArg f :: HasArgResult where
  HasArg (_ -> _) = 'ArgPresent
  HasArg _ = 'NoArg

class ReorderArgs (fHasArg :: HasArgResult) f g where
  reorderArgsImpl :: Proxy fHasArg -> f -> g

instance
    ( matches ~ HasAMatch a (b -> y) (b -> y)
    , result ~ ApplyByTypeResult matches a (b -> y)
    , ApplyByType matches a (b -> y)
    , ReorderArgs (HasArg result) result x
    ) => ReorderArgs 'ArgPresent (b -> y) (a -> x) where
  reorderArgsImpl _ f x =
    reorderArgsImpl (Proxy :: Proxy (HasArg result)) $
    applyByType f x
  {-# INLINE reorderArgsImpl #-}

instance r1 ~ r2 => ReorderArgs 'NoArg r1 r2 where
  reorderArgsImpl _ f = f
  {-# INLINE reorderArgsImpl #-}

class ReorderUniqueArgs (fHasArg :: HasArgResult) f g where
  reorderUniqueArgsImpl :: Proxy fHasArg -> f -> g

instance
    ( matches ~ HasUniqueMatch a (b -> y) (b -> y)
    , result ~ ApplyByTypeResult matches a (b -> y)
    , ApplyByType matches a (b -> y)
    , ReorderUniqueArgs (HasArg result) result x
    ) => ReorderUniqueArgs 'ArgPresent (b -> y) (a -> x) where
  reorderUniqueArgsImpl _ f x =
    reorderUniqueArgsImpl (Proxy :: Proxy (HasArg result)) $
    applyByUniqueType f x
  {-# INLINE reorderUniqueArgsImpl #-}

instance r1 ~ r2 => ReorderUniqueArgs 'NoArg r1 r2 where
  reorderUniqueArgsImpl _ f = f
  {-# INLINE reorderUniqueArgsImpl #-}

--------------------------------------------------------------------------------
-- Type error messages

type NoMatchError a f =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " does not occur in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via type directed application."

type NoMatchForResultError a r =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " does not occur in the arguments of the function that returns " ':$$:
  'Text "  " ':<>: 'ShowType r ':$$:
  'Text "and so cannot be applied via type directed application."

type AmbiguousMatchError a f =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " occurs multiple times in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via unique type directed application."
