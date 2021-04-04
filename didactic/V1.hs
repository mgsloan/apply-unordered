{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module V1 where

infixl 1 ?

class ApplyByType a f where
  type ApplyByTypeResult a f
  (?) :: f -> a -> ApplyByTypeResult a f

instance ApplyByType a (a -> r) where
  type ApplyByTypeResult a (a -> r) = r
  f ? x = f x

instance ApplyByType a r => ApplyByType a (b -> r) where
  type ApplyByTypeResult a (b -> r) = b -> ApplyByTypeResult a r
  f ? y = \x -> f x ? y
