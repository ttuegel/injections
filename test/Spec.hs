{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Injection

import Data.Dynamic (Dynamic)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Numeric.Natural (Natural)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

main :: IO ()
main = hspec $ do
    describe "instance Injection Integer Integer" $ do
        it "is resolvable" (resolveInjection @Integer @Integer)
    describe "instance Injection String String" $ do
        it "is resolvable" (resolveInjection @String @String)
    describe "instance Injection Text Text" $ do
        it "is resolvable" (resolveInjection @Text @Text)
    describe "instance Injection Lazy.Text Lazy.Text" $ do
        it "is resolvable" (resolveInjection @Lazy.Text @Lazy.Text)
    describe "instance Retraction Integer Dynamic" $ do
        it "is resolvable" (resolveRetraction @Integer @Dynamic)
        it "is the left inverse of inject" (lawLeftInverse @Integer @Dynamic)
    describe "instance Injection (Maybe Integer) [Integer]" $ do
        it "is resolvable" (resolveInjection @(Maybe Integer) @[Integer])
        it "outputs a value with the same length as the input" $ do
            let test = inject @(Maybe Integer) @[Integer]
            length (test Nothing) `shouldBe` 0
            length (test (Just 1)) `shouldBe` 1
        it "is injective" (lawInjective @(Maybe Integer) @[Integer])
    describe "instance Retraction (Maybe Integer) [Integer]" $ do
        it "is resolvable" (resolveRetraction @(Maybe Integer) @[Integer])
        it "is the left inverse of inject" (lawLeftInverse @(Maybe Integer) @[Integer])
    describe "instance Injection Natural Integer" $ do
        it "is resolvable" (resolveInjection @Natural @Integer)
        it "injects the value itself" $ do
            let test = inject @Natural @Integer
            test 0 `shouldBe` 0
            test 1 `shouldBe` 1
            test 2 `shouldBe` 2
        it "is injective" (lawInjective @Natural @Integer)
    describe "instance Retraction Natural Integer" $ do
        it "is resolvable" (resolveRetraction @Natural @Integer)
        it "is the left inverse of inject" (lawLeftInverse @Natural @Integer)
        it "is defined over non-negative integers" $ do
            retract @Natural @Integer 0 `shouldBe` Just 0
            retract @Natural @Integer 1 `shouldBe` Just 1
        it "is not defined over negative integers" $ do
            retract @Natural @Integer (-1) `shouldBe` Nothing
    describe "instance Injection String Text" $ do
        it "is resolvable" (resolveInjection @String @Text)
        it "is injective" (lawInjective @String @Text)
    describe "instance Injection String Lazy.Text" $ do
        it "is resolvable" (resolveInjection @String @Lazy.Text)
        it "is injective" (lawInjective @String @Lazy.Text)

resolveInjection :: forall from into. Injection from into => Expectation
resolveInjection = seq (inject @from @into) return ()

resolveRetraction :: forall from into. Retraction from into => Expectation
resolveRetraction = seq (retract @from @into) return ()

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
