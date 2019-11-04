{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Main where

import Control.Apply.Positional
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

replicateChar :: Int -> Char -> String
replicateChar = replicate

main :: IO ()
main = hspec $ do
  describe "Positional application" $ do
    it "handles simple functions" $
      applyN @1 replicateChar 'c' 3 `shouldBe` "ccc"
    it "handles polymorphic functions" $
      applyN @1 div 2 4 `shouldBe` 2
