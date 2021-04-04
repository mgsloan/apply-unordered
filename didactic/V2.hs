{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module V2 where

type family ApplyByTypeResult a f where
  ApplyByTypeResult a (a -> r) = r
  ApplyByTypeResult a (b -> r) = b -> ApplyByTypeResult a r

infixl 1 ?

class ApplyByType a f where
  (?) :: f -> a -> ApplyByTypeResult a f

instance ApplyByType a (a -> r) where
  f ? x = f x

instance ApplyByType a r => ApplyByType a (b -> r) where
  f ? y = \x -> f x ? y
