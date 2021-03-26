{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Proxy (Proxy(..))

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

infixl 1 ?

(?)
  :: forall matches a f.
     ( matches ~ MatchFirstArg a f
     , ApplyByType matches a f
     )
  => f -> a -> ApplyByTypeResult matches a f
(?) = applyByTypeImpl (Proxy :: Proxy matches)

-- Usage

replicateChars :: Int -> Char -> String
replicateChars = replicate

main :: IO ()
main = print (replicateChars ? 'a' ? (10 :: Int))
