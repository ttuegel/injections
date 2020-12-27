module Main (main) where

import Injection

import Data.Dynamic
import Data.Foldable
import Numeric.Natural
import Test.Hspec

main :: IO ()
main = hspec $ do
    let integers :: [Integer]
        integers = [-8..8]
        naturals :: [Natural]
        naturals = [0..16]
    describe "retract :: Integer -> Dynamic" $ do
        it "is the left inverse of inject" $ do
            let test = retract . inject @_ @Dynamic
            for_ integers $ \input -> test input `shouldBe` Just input
            retract @Integer (inject @Integer @Dynamic 1) `shouldBe` Just 1
    describe "inject :: Maybe Integer -> [Integer]" $ do
        it "outputs a value with the same length as the input" $ do
            let test = inject @(Maybe Integer) @[Integer]
            length (test Nothing) `shouldBe` 0
            length (test (Just 1)) `shouldBe` 1
    describe "retract :: [Integer] -> Maybe (Maybe Integer)" $ do
        it "is the left inverse of inject" $ do
            let test = retract . inject @_ @[Integer]
                inputs = Nothing : map Just integers
            for_ inputs $ \input -> test input `shouldBe` Just input
    describe "inject :: Natural -> Integer" $ do
        it "injects the value itself" $ do
            let test = inject @Natural @Integer
            test 0 `shouldBe` 0
            test 1 `shouldBe` 1
            test 2 `shouldBe` 2
    describe "retract :: Integer -> Maybe Natural" $ do
        it "is the left inverse of inject" $ do
            let test = retract . inject @_ @Integer
            for_ naturals $ \natural -> test natural `shouldBe` Just natural
        it "retracts non-negative integers" $ do
            let test = retract @Natural @Integer
            test 0 `shouldBe` Just 0
            test 1 `shouldBe` Just 1
            test 2 `shouldBe` Just 2
        it "is not defined over negative integers" $ do
            retract @Natural @Integer (-1) `shouldBe` Nothing
