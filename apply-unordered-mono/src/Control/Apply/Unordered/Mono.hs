{-# LANGUAGE UndecidableInstances #-}

-- | This modules is an experiment in type family / typeclass
-- machinery to provide type-directed function application and
-- argument reordering. It is severely restricted, in that it only
-- supports monomorphic arguments and parameters.
--
-- Few commonly known functions in base are monomorphic with multiple
-- arguments, so I will use an example from @ Data.Text @:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Data.Text as T
-- >
-- > ex1 :: Int -> T.Text
-- > ex1 = T.replicate ? ("wow! " :: Text)
--
-- Cool! The '(?)' operator has magically provided the 2nd argument of
-- @T.replicate@ (its type is @Int -> Text -> Text@).
--
-- This module also provides `reorderArgs`, which does this variety of
-- function application in a variadic fashion.
--
-- > ex2 :: T.Text -> Int -> T.Text
-- > ex2 = reorderArgs T.replicate
module Control.Apply.Unordered.Mono
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
    -- * Machinery for reordering parameters
  , HasArgResult(..)
  , HasArg
  , ReorderArgs
  , ReorderUniqueArgs
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- | Applies a function to an argument, by providing the argument to
-- the first parameter which matches the argument type. This does not
-- handle any polymorphism in the function type or any argument types.
applyByType
  :: forall matches a f.
     ( matches ~ HasAMatch a f f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
applyByType = applyByTypeImpl (Proxy :: Proxy matches)
{-# INLINE applyByType #-}

infixl 1 ?

-- | Operator alias for 'applyByType'
(?)
  :: forall matches a f.
     ( matches ~ HasAMatch a f f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
(?) = applyByType
{-# INLINE (?) #-}

-- | Similarly to 'applyByType', applies a function to an argument by
-- matching the argument type with the parameter type. If the match is
-- ambiguous, this is a type error.
applyByUniqueType
  :: forall matches a f.
     ( matches ~ HasUniqueMatch a f f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
applyByUniqueType = applyByTypeImpl (Proxy :: Proxy matches)
{-# INLINE applyByUniqueType #-}

infixl 1 ?!

-- | Operator alias for 'applyByUniqueType'
(?!)
  :: forall matches a f.
     ( matches ~ HasUniqueMatch a f f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
(?!) = applyByUniqueType
{-# INLINE (?!) #-}

reorderArgs :: forall f g. ReorderArgs (HasArg f) f g => f -> g
reorderArgs = reorderArgsImpl (Proxy :: Proxy (HasArg f))
{-# INLINE reorderArgs #-}

reorderUniqueArgs :: forall f g. ReorderUniqueArgs (HasArg f) f g => f -> g
reorderUniqueArgs = reorderUniqueArgsImpl (Proxy :: Proxy (HasArg f))
{-# INLINE reorderUniqueArgs #-}

--------------------------------------------------------------------------------
-- Match machinery

-- | A data-kind for 'MatchArgResult' to return.
data MatchArgResult
  = Matches         -- ^ Indicates the first argument matches.
  | Doesn'tMatch    -- ^ Indicates the first argument doesn't match.
  | NoArgToMatch    -- ^ Indicates the function has no first argument.

-- | A type family which checks if the specified argument type matches
-- the first parameter of a function.
type family MatchFirstArg a f :: MatchArgResult where
  MatchFirstArg a (a -> _) = 'Matches
  MatchFirstArg a (b -> _) = 'Doesn'tMatch
  MatchFirstArg _ _ = 'NoArgToMatch

-- | A type family which returns a 'TypeError', specifically
-- 'NoMatchErrorMsg', if the specified argument type matches none of
-- the function parameter types. Otherwise, it behaves the same as
-- 'MatchFirstArg'.
type family HasAMatch a f f0 :: MatchArgResult where
  HasAMatch a (a -> _) f0 = MatchFirstArg a f0
  HasAMatch a (b -> r) f0 = HasAMatch a r f0
  HasAMatch a _ f0 = TypeError (NoMatchErrorMsg a f0)

-- | Typeclass used to implement 'applyByType'. The first type
-- argument is used to select instances, and should always be
-- @MatchFirstArg a f@.
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

instance TypeError (NoMatchForResultErrorMsg a r)
      => ApplyByType 'NoArgToMatch a r where
  type ApplyByTypeResult 'NoArgToMatch a r =
    TypeError (NoMatchForResultErrorMsg a r)
  applyByTypeImpl = error "impossible"

--------------------------------------------------------------------------------
-- Unique match machinery

-- | A type family similar to 'HasAMatch', but also checks that
-- there's a unique match for the argument type.
type family HasUniqueMatch a f f0 :: MatchArgResult where
  HasUniqueMatch a (a -> r) f0 = CheckAmbiguousMatch a r f0
  HasUniqueMatch a (b -> r) f0 = HasUniqueMatch a r f0
  HasUniqueMatch a _ f0 = TypeError (NoMatchErrorMsg a f0)

-- | Used to implement 'HasUniqueMatch', this type family is used
-- after the match is found. Any additional matches cause an
-- 'AmbiguousMatchErrorMsg'.
type family CheckAmbiguousMatch a f f0 :: MatchArgResult where
  CheckAmbiguousMatch a (a -> _) f0 = TypeError (AmbiguousMatchErrorMsg a f0)
  CheckAmbiguousMatch a (b -> r) f0 = CheckAmbiguousMatch a r f0
  CheckAmbiguousMatch a _ f0 = MatchFirstArg a f0

--------------------------------------------------------------------------------
-- Argument reordering machinery

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

-- | Typeclass used to implement 'reorderUniqueArgs'. The first type
-- argument is used to select instances, and should always be @HasArg
-- f@.
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

type NoMatchErrorMsg a f =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " does not occur in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via type directed application."

type NoMatchForResultErrorMsg a r =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " does not occur in the arguments of the function that returns " ':$$:
  'Text "  " ':<>: 'ShowType r ':$$:
  'Text "and so cannot be applied via type directed application."

type AmbiguousMatchErrorMsg a f =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " occurs multiple times in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via unique type directed application."
