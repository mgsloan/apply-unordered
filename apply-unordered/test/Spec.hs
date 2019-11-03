{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Main where

import Control.Apply.Unordered
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

replicateChar :: Int -> Char -> String
replicateChar = replicate

polymorphicReturn :: Int -> Char -> Maybe a
polymorphicReturn _ _ = Nothing

three :: Int
three = 3

main :: IO ()
main = hspec $ do
  describe "Type directed apply with first match" $ do
    it "handles simple parameters (1)" $
      (replicateChar ? 'a' ? three) `shouldBe` "aaa"
    it "handles simple parameters (2)" $
      (replicateChar ? three ? 'a') `shouldBe` "aaa"
    it "handles simple ambiguous parameters (1)" $
      ((||) ? True ? False) `shouldBe` True
    it "handles simple ambiguous parameters (2)" $
      ((||) ? False ? False) `shouldBe` False
    it "errors when there is no match" $
      shouldNotTypecheck ((||) ? "test")
    it "doesn't handle any parameter polymorphism" $
      shouldNotTypecheck (replicate ? 3 $ 'a' :: String)
    it "does handle result polymorphism" $
      (polymorphicReturn ? three $ 'a') `shouldBe` Nothing

  describe "Type directed apply with unique match" $ do
    it "handles simple parameters (1)" $
      (replicateChar ?! 'a' ?! three) `shouldBe` "aaa"
    it "handles simple parameters (2)" $
      (replicateChar ?! three ?! 'a') `shouldBe` "aaa"
    it "checks for ambiguity" $
      shouldNotTypecheck ((||) ?! True ?! False :: Bool)
    it "errors when there is no match" $
      shouldNotTypecheck (replicateChar ? "test")
