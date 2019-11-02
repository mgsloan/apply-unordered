{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.UnorderedApply
  (
    -- * Functions for type directed curry which matches first parameter
    applyFirst1
  , applyFirstN
    -- * Functions for type directed curry which matches unique parameter
  , apply1
  , applyN
    -- * Machinery for type directed curry which matches first parameter
  , CheckMatchFirst
    -- * Machinery for type directed curry which matches unique parameter
  , CheckAmbiguity
  , HasOneMatch
  , HasNoMatch
  ) where

import Data.Proxy
import GHC.TypeLits (TypeError, ErrorMessage(..))

data MatchMode = Unique | First

data MatchResult = NoMatchFirst | NoMatch | AmbiguousMatch | Match

type family CheckMatchFirst (mode :: MatchMode) f a :: MatchResult where
  CheckMatchFirst 'Unique (a -> r) a = CheckAmbiguity r a
  CheckMatchFirst 'First (a -> r) a = 'Match
  CheckMatchFirst _ (a -> r) b = 'NoMatchFirst
  CheckMatchFirst _ _ _ = 'NoMatch

type family CheckAmbiguity f a :: MatchResult where
  CheckAmbiguity (a -> r) a = 'AmbiguousMatch
  CheckAmbiguity (a -> r) b = CheckAmbiguity r b
  CheckAmbiguity _ _ = 'Match

-- TODO: explain
type family HasOneMatch f0 f a :: MatchResult where
  HasOneMatch f0 (a -> r) a = HasNoMatch f0 r a
  HasOneMatch f0 (a -> r) b = HasOneMatch f0 r b
  HasOneMatch f0 _ a = TypeError (NoMatchError f0 a)

type family HasNoMatch f0 f a :: MatchResult where
  HasNoMatch f0 (a -> r) a = TypeError (AmbiguousError f0 a)
  HasNoMatch f0 (a -> r) b = HasNoMatch f0 r b
  HasNoMatch f0 _ a = CheckMatchFirst 'Unique f0 a

class TypeDirectedCurry (mode :: MatchMode) (matches :: MatchResult) f a where
  type Result mode matches f a
  applyInternal
    :: Proxy mode
    -> Proxy matches
    -> f
    -> a
    -> Result mode matches f a

instance TypeDirectedCurry mode 'Match (a -> r) a where
  type Result mode 'Match (a -> r) a = r
  applyInternal _ _ f x = f x
  {-# INLINE applyInternal #-}

instance TypeDirectedCurry mode (CheckMatchFirst mode r a) r a =>
         TypeDirectedCurry mode 'NoMatchFirst (b -> r) a where
  type Result mode 'NoMatchFirst (b -> r) a =
    b -> Result mode (CheckMatchFirst mode r a) r a
  applyInternal mode _ f y =
    \x -> applyInternal mode (Proxy :: Proxy (CheckMatchFirst mode r a)) (f x) y
  {-# INLINE applyInternal #-}

type AmbiguousError f a =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " occurs multiple times in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via type directed currying."

instance TypeError (AmbiguousError (b -> r) a) => TypeDirectedCurry mode 'AmbiguousMatch (b -> r) a where
  type Result mode 'AmbiguousMatch (b -> r) a = TypeError (AmbiguousError (b -> r) a)
  applyInternal = error "impossible"

type NoMatchError f a =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " does not occur in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via type directed currying."

instance TypeError (NoMatchError f a) => TypeDirectedCurry mode 'NoMatch f a where
  type Result mode 'NoMatch f a = TypeError (NoMatchError f a)
  applyInternal = error "impossible"

apply1
  :: forall matches f a.
     ( matches ~ HasOneMatch f f a
     , TypeDirectedCurry 'Unique matches f a
     )
  => f -> a -> Result 'Unique matches f a
apply1 = applyInternal (Proxy :: Proxy 'Unique) (Proxy :: Proxy matches)
{-# INLINE apply1 #-}

applyFirst1
  :: forall matches f a.
     ( matches ~ CheckMatchFirst 'First f a
     , TypeDirectedCurry 'First matches f a
     )
  => f -> a -> Result 'First matches f a
applyFirst1 = applyInternal (Proxy :: Proxy 'First) (Proxy :: Proxy matches)
{-# INLINE applyFirst1 #-}

applyN :: forall f g. MultiCurry 'Unique (IsMore f) f g => f -> g
applyN = multiCurry (Proxy :: Proxy 'Unique) (Proxy :: Proxy (IsMore f))
{-# INLINE applyN #-}

applyFirstN :: forall f g. MultiCurry 'First (IsMore f) f g => f -> g
applyFirstN = multiCurry (Proxy :: Proxy 'First) (Proxy :: Proxy (IsMore f))
{-# INLINE applyFirstN #-}

data MultiCurryState = More | Done

class MultiCurry (mode :: MatchMode) (isFunction :: MultiCurryState) f g where
  multiCurry :: Proxy mode -> Proxy isFunction -> f -> g

instance
    ( matches ~ HasOneMatch (a -> x) (a -> x) b
    , result ~ Result 'Unique matches (a -> x) b
    , TypeDirectedCurry 'Unique matches (a -> x) b
    , MultiCurry 'Unique (IsMore result) result y
    ) => MultiCurry 'Unique 'More (a -> x) (b -> y) where
  multiCurry mode _ f x =
    multiCurry mode (Proxy :: Proxy (IsMore result)) $
    apply1 f x
  {-# INLINE multiCurry #-}

instance
    ( matches ~ CheckMatchFirst 'First (a -> x) b
    , result ~ Result 'First matches (a -> x) b
    , TypeDirectedCurry 'First matches (a -> x) b
    , MultiCurry 'First (IsMore result) result y
    ) => MultiCurry 'First 'More (a -> x) (b -> y) where
  multiCurry mode _ f x =
    multiCurry mode (Proxy :: Proxy (IsMore result)) $
    applyFirst1 f x
  {-# INLINE multiCurry #-}

-- instance MultiCurry 'False r r where
instance r1 ~ r2 => MultiCurry mode 'Done r1 r2 where
  multiCurry _ _ f = f
  {-# INLINE multiCurry #-}

type family IsMore f where
  IsMore (_ -> _) = 'More
  IsMore _ = 'Done
