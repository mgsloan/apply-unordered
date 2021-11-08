{-# language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GADTs #-}

-- | Copied verbatim from lightandlight's reddit comment
--
-- https://www.reddit.com/r/haskell/comments/mk11iu/comment/gtemzu9/?utm_source=share&utm_medium=web2x&context=3
module Main where

class Apply f x y | f x -> y where
  apply :: f -> x -> y

instance {-# overlappable #-} (x ~ (a -> b)) => Apply x a b where
  apply = ($)

instance {-# overlapping #-} Apply b a b' => Apply (a' -> b) a (a' -> b') where
  apply f a = \a' -> apply (f a') a

infixl 5 ?
(?) :: Apply f x y => f -> x -> y
(?) = apply

times :: Int -> String -> String
times n s = take (n * length s) $ cycle s

main :: IO ()
main = do
  putStrLn $ times ? 5 ? "hi"
  putStrLn $ times ? "hi" ? 5
