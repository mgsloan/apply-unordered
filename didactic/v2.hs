{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

type family ApplyByTypeResult a f where
  ApplyByTypeResult a (a -> r) = r
  ApplyByTypeResult a (b -> r) = b -> ApplyByTypeResult a r

class ApplyByType a f where
  applyByTypeImpl :: f -> a -> ApplyByTypeResult a f

instance ApplyByType a (a -> r) where
  applyByTypeImpl f x = f x

instance ApplyByType a r => ApplyByType a (b -> r) where
  applyByTypeImpl f y = \x -> applyByTypeImpl (f x) y
