{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Proxy (Proxy(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))

data MatchArgResult
  = Matches
  | Doesn'tMatch
  | NoArgToMatch

type family MatchFirstArg a f :: MatchArgResult where
  MatchFirstArg a (a -> r) = 'Matches
  MatchFirstArg a (b -> r) = 'Doesn'tMatch
  MatchFirstArg _ _ = 'NoArgToMatch

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

instance ApplyByType (MatchFirstArg a r) a r
      => ApplyByType 'Doesn'tMatch a (b -> r) where
  type ApplyByTypeResult 'Doesn'tMatch a (b -> r) =
    b -> ApplyByTypeResult (MatchFirstArg a r) a r
  applyByTypeImpl _ f y =
    \x -> applyByTypeImpl (Proxy :: Proxy (MatchFirstArg a r)) (f x) y

instance TypeError (NoMatchForResultErrorMsg a r)
      => ApplyByType 'NoArgToMatch a r where
  type ApplyByTypeResult 'NoArgToMatch a r =
    TypeError (NoMatchForResultErrorMsg a r)
  applyByTypeImpl = error "impossible"

type NoMatchForResultErrorMsg a r =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " does not occur in the arguments of the function that returns " ':$$:
  'Text "  " ':<>: 'ShowType r ':$$:
  'Text "and so cannot be applied via type directed application."

type family HasAMatch a f f0 :: MatchArgResult where
  HasAMatch a (a -> r) f0 = MatchFirstArg a f0
  HasAMatch a (b -> r) f0 = HasAMatch a r f0
  HasAMatch a _ f0 = TypeError (NoMatchErrorMsg a f0)

type NoMatchErrorMsg a f =
  'Text "Parameter type " ':<>:
  'ShowType a ':<>:
  'Text " does not occur in the arguments of the function type " ':$$:
  'Text "  " ':<>: 'ShowType f ':$$:
  'Text "and so cannot be applied via type directed application."

applyByType
  :: forall matches a f.
     ( matches ~ HasAMatch a f f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
applyByType = applyByTypeImpl (Proxy :: Proxy matches)

-- Demo of type error

replicateChars :: Int -> Char -> String
replicateChars = replicate

shouldBeATypeError = replicateChars `applyByType` "a" :: Int -> String
