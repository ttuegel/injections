{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Injection

import Data.Dynamic (Dynamic)
import Numeric.Natural (Natural)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec $ do
    describe "retract @Integer @Dynamic" $ do
        it "is the left inverse of inject" (lawLeftInverse @Integer @Dynamic)
    describe "inject @(Maybe Integer) @[Integer]" $ do
        it "outputs a value with the same length as the input" $ do
            let test = inject @(Maybe Integer) @[Integer]
            length (test Nothing) `shouldBe` 0
            length (test (Just 1)) `shouldBe` 1
        it "is injective" (lawInjective @(Maybe Integer) @[Integer])
    describe "retract @(Maybe Integer) @[Integer]" $ do
        it "is the left inverse of inject" (lawLeftInverse @(Maybe Integer) @[Integer])
    describe "inject @Natural @Integer" $ do
        it "injects the value itself" $ do
            let test = inject @Natural @Integer
            test 0 `shouldBe` 0
            test 1 `shouldBe` 1
            test 2 `shouldBe` 2
        it "is injective" (lawInjective @Natural @Integer)
    describe "retract @Natural @Integer" $ do
        it "is the left inverse of inject" (lawLeftInverse @Natural @Integer)
        it "is defined over non-negative integers" $ do
            retract @Natural @Integer 0 `shouldBe` Just 0
            retract @Natural @Integer 1 `shouldBe` Just 1
        it "is not defined over negative integers" $ do
            retract @Natural @Integer (-1) `shouldBe` Nothing

lawInjective
    :: forall from into
    .  Injection from into
    => Arbitrary from
    => (Eq from, Show from)
    => (Eq into, Show into)
    => Property
lawInjective = property $ \from1 from2 ->
    let into1 = inject @from @into from1
        into2 = inject @from @into from2
    in (from1 /= from2) ==> (into1 =/= into2)

lawLeftInverse
    :: forall from into
    .  Retraction from into
    => Arbitrary from
    => (Eq from, Show from)
    => Property
lawLeftInverse = property $ \from ->
    let into = inject @from @into from
        from' = retract @from @into into
    in Just from === from'
