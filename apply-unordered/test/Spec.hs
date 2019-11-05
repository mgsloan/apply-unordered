{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Main where

import Control.Apply.Unordered
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

replicateChar :: Int -> Char -> String
replicateChar = replicate

polymorphicReturn :: Int -> Char -> Maybe a
polymorphicReturn _ _ = Nothing

threeArgs :: Int -> Char -> Bool -> String
threeArgs a b c = show (a, b, c)

i3 :: Int
i3 = 3

main :: IO ()
main = hspec $ do
  describe "Type directed apply with first match" $ do
    it "handles simple parameters (1)" $
      (replicateChar ? 'a' ? i3) `shouldBe` "aaa"
    it "handles simple parameters (2)" $
      (replicateChar ? i3 ? 'a') `shouldBe` "aaa"
    it "handles simple ambiguous parameters (1)" $
      ((||) ? True ? False) `shouldBe` True
    it "handles simple ambiguous parameters (2)" $
      ((||) ? False ? False) `shouldBe` False
    it "errors when there is no match" $
      shouldNotTypecheck ((||) ? "test")
    it "doesn't handle any parameter polymorphism" $
      shouldNotTypecheck (replicate ? 3 $ 'a' :: String)
    it "does handle result polymorphism" $
      (polymorphicReturn ? i3 $ 'a') `shouldBe` Nothing

  describe "Type directed apply with unique match" $ do
    it "handles simple parameters (1)" $
      (replicateChar ?! 'a' ?! i3) `shouldBe` "aaa"
    it "handles simple parameters (2)" $
      (replicateChar ?! i3 ?! 'a') `shouldBe` "aaa"
    it "checks for ambiguity" $
      shouldNotTypecheck ((||) ?! True ?! False :: Bool)
    it "errors when there is no match" $
      shouldNotTypecheck (replicateChar ? "test")

  describe "Argument reordering" $ do
    it "can re-order a 3 argument function (1)" $
      reorderArgs threeArgs 'a' i3 True `shouldBe` "(3,'a',True)"
    it "can re-order a 3 argument function (2)" $
      reorderArgs threeArgs False 'b' i3 `shouldBe` "(3,'b',False)"
