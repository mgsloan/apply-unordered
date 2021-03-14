{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

class ApplyByType a f where
  type ApplyByTypeResult a f
  applyByTypeImpl :: f -> a -> ApplyByTypeResult a f

instance ApplyByType a (a -> r) where
  type ApplyByTypeResult a (a -> r) = r
  applyByTypeImpl f x = f x

instance ApplyByType a r => ApplyByType a (b -> r) where
  type ApplyByTypeResult a (b -> r) = ApplyByTypeResult a r
  applyByTypeImpl f y = \x -> applyByTypeImpl (f x) y
