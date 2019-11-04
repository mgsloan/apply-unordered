
apply1
  :: forall a f. ApplyAt (FromGHC 1) a f
  => f -> a -> ApplyAtResult (FromGHC 1) a f
apply1 = applyN @ 1
{-# INLINE apply1 #-}

apply2
  :: forall a f. ApplyAt (FromGHC 2) a f
  => f -> a -> ApplyAtResult (FromGHC 2) a f
apply2 = applyN @ 2
{-# INLINE apply2 #-}

apply2
  :: forall a f. ApplyAt (FromGHC 3) a f
  => f -> a -> ApplyAtResult (FromGHC 3) a f
apply2 = applyN @ 3
{-# INLINE apply3 #-}
