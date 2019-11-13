{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fplugin=Control.Apply.Unordered.Plugin #-}

module Main where

import Control.Apply.Positional
import Control.Apply.Unordered
import Data.Proxy
import GHC.TypeLits
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
    it "yields type error for out of bounds nats" $
      shouldNotTypecheck (applyN @2 replicateChar 'c' 3 :: String)
  describe "Unordered apply via plugin" $ do
    it "Can match types of a polymorphic function" $
      (replicate ? 'c' ? (3 :: Int)) `shouldBe` "ccc"
