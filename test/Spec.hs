{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Main where

import Control.UnorderedApply
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

replicateChar :: Int -> Char -> String
replicateChar = replicate

main :: IO ()
main = hspec $ do
  describe "Type directed apply with first match" $ do
    it "handles simple parameters (1)" $
      (replicateChar ? 'a' ? 3) `shouldBe` "aaa"
    it "handles simple parameters (2)" $
      (replicateChar ? 3 ? 'a') `shouldBe` "aaa"
    it "handles simple ambiguous parameters (1)" $
      ((||) ? True ? False) `shouldBe` True
    it "handles simple ambiguous parameters (2)" $
      ((||) ? False ? False) `shouldBe` False
    it "errors when there is no match" $
      shouldNotTypecheck ((||) ? "test")

  describe "Type directed apply with unique match" $ do
    it "handles simple parameters (1)" $
      (replicateChar ?! 'a' ?! 3) `shouldBe` "aaa"
    it "handles simple parameters (2)" $
      (replicateChar ?! 3 ?! 'a') `shouldBe` "aaa"
    it "checks for ambiguity" $
      shouldNotTypecheck ((||) ?! True ?! False :: Bool)
    it "errors when there is no match" $
      shouldNotTypecheck (replicateChar ? "test")
