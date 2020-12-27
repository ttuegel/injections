module Main (main) where

import Injection

import Data.Dynamic
import Numeric.Natural
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "retract :: Integer -> Dynamic" $ do
        it "is the left inverse of inject" $ do
            retract @Integer (inject @Integer @Dynamic 1) `shouldBe` Just 1
    describe "retract :: [Integer] -> Maybe (Maybe Integer)" $ do
        it "is the left inverse of inject" $ do
            retract @(Maybe Integer) (inject @(Maybe Integer) @[Integer] Nothing) `shouldBe` Just Nothing
            retract @(Maybe Integer) (inject @(Maybe Integer) @[Integer] (Just 1)) `shouldBe` Just (Just 1)
    describe "inject :: Natural -> Integer" $ do
        it "injects the value itself" $ do
            inject @Natural 0 `shouldBe` (0 :: Integer)
            inject @Natural 1 `shouldBe` (1 :: Integer)
            inject @Natural 2 `shouldBe` (2 :: Integer)
    describe "retract :: Integer -> Maybe Natural" $ do
        it "retracts non-negative integers" $ do
            retract @Natural @Integer 0 `shouldBe` Just 0
            retract @Natural @Integer 1 `shouldBe` Just 1
            retract @Natural @Integer 2 `shouldBe` Just 2
        it "is not defined over negative integers" $ do
            retract @Natural @Integer (-1) `shouldBe` Nothing
