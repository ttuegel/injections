{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Injection

import Data.Complex (Complex ((:+)))
import Data.Dynamic (Dynamic)
import Data.Fixed (Fixed, E6)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (Product)
import Data.Monoid (Sum)
import Data.Ord (Down (..))
import Data.Ratio (Ratio, (%))
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Numeric.Natural (Natural)
import Test.Hspec
import Test.QuickCheck hiding (Fixed)
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
    describe "instance Injection Text String" $ do
        it "is resolvable" (resolveInjection @Text @String)
        it "is injective" (lawInjective @Text @String)
    describe "instance Injection Lazy.Text String" $ do
        it "is resolvable" (resolveInjection @Lazy.Text @String)
        it "is injective" (lawInjective @Lazy.Text @String)
    describe "instance Injection Text Lazy.Text" $ do
        it "is resolvable" (resolveInjection @Text @Lazy.Text)
        it "is injective" (lawInjective @Text @Lazy.Text)
    describe "instance Injection Lazy.Text Text" $ do
        it "is resolvable" (resolveInjection @Lazy.Text @Text)
        it "is injective" (lawInjective @Lazy.Text @Text)
    describe "instance Injection Integer (Fixed a)" $ do
        it "is resolvable" (resolveInjection @Integer @(Fixed E6))
        it "is injective" (lawInjective @Integer @(Fixed E6))
    describe "instance Retraction Integer (Fixed a)" $ do
        it "is resolvable" (resolveRetraction @Integer @(Fixed E6))
        it "is the left inverse of inject" (lawLeftInverse @Integer @(Fixed E6))
        it "is defined over integers" $ do
            retract @Integer @(Fixed E6) 0 `shouldBe` Just 0
            retract @Integer @(Fixed E6) 1 `shouldBe` Just 1
            retract @Integer @(Fixed E6) (-1) `shouldBe` Just (-1)
        it "is not defined over fractions" $ do
            retract @Integer @(Fixed E6) (1 / 2) `shouldBe` Nothing
    describe "instance Injection Integer (Const Integer String)" $ do
        it "is resolvable" (resolveInjection @Integer @(Const Integer String))
        it "is injective" (lawInjective @Integer @(Const Integer String))
    describe "instance Injection (Const Integer String) Integer" $ do
        it "is resolvable" (resolveInjection @(Const Integer String) @Integer)
        it "is injective" (lawInjective @(Const Integer String) @Integer)
    describe "instance Injection Integer (Ratio Integer)" $ do
        it "is resolvable" (resolveInjection @Integer @(Ratio Integer))
        it "is injective" (lawInjective @Integer @(Ratio Integer))
    describe "instance Retraction Integer (Ratio Integer)" $ do
        it "is resolvable" (resolveRetraction @Integer @(Ratio Integer))
        it "is the left inverse of inject" (lawLeftInverse @Integer @(Ratio Integer))
        it "is defined over integers" $ do
            retract @Integer @(Ratio Integer) (0 % 1) `shouldBe` Just 0
            retract @Integer @(Ratio Integer) (1 % 1) `shouldBe` Just 1
            retract @Integer @(Ratio Integer) (1 % (-1)) `shouldBe` Just (-1)
        it "is not defined over fractions" $ do
            retract @Integer @(Ratio Integer) (1 % 2) `shouldBe` Nothing
    describe "instance Injection Double (Complex Double)" $ do
        it "is resolvable" (resolveInjection @Double @(Complex Double))
        it "is injective" (lawInjective @Double @(Complex Double))
    describe "instance Retraction Double (Complex Double)" $ do
        it "is resolvable" (resolveRetraction @Double @(Complex Double))
        it "is the left inverse of inject" (lawLeftInverse @Double @(Complex Double))
        it "is defined over real numbers" $ do
            retract @Double @(Complex Double) (0 :+ 0) `shouldBe` Just 0
            retract @Double @(Complex Double) (1 :+ 0) `shouldBe` Just 1
            retract @Double @(Complex Double) ((-1) :+ 0) `shouldBe` Just (-1)
        it "is not defined over imaginary and complex numbers" $ do
            retract @Double @(Complex Double) (0 :+ 1) `shouldBe` Nothing
            retract @Double @(Complex Double) (1 :+ 1) `shouldBe` Nothing
    describe "instance Injection Integer (Identity Integer)" $ do
        it "is resolvable" (resolveInjection @Integer @(Identity Integer))
        it "is injective" (lawInjective @Integer @(Identity Integer))
    describe "instance Injection (Identity Integer) Integer" $ do
        it "is resolvable" (resolveInjection @(Identity Integer) @Integer)
        it "is injective" (lawInjective @(Identity Integer) @Integer)
    describe "instance Injection (NonEmpty Integer) [Integer]" $ do
        it "is resolvable" (resolveInjection @(NonEmpty Integer) @[Integer])
        it "is injective" (lawInjective @(NonEmpty Integer) @[Integer])
    describe "instance Retraction (NonEmpty Integer) [Integer]" $ do
        it "is resolvable" (resolveRetraction @(NonEmpty Integer) @[Integer])
        it "is the left inverse of inject" (lawLeftInverse @(NonEmpty Integer) @[Integer])
        it "is not defined on the empty list" $ do
            retract @(NonEmpty Integer) @[Integer] [] `shouldBe` Nothing
        it "is defined over non-empty lists" $ do
            retract @(NonEmpty Integer) @[Integer] [0] `shouldBe` Just (0 :| [])
            retract @(NonEmpty Integer) @[Integer] [0, 1] `shouldBe` Just (0 :| [1])
    describe "instance Injection Integer (Down Integer)" $ do
        it "is resolvable" (resolveInjection @Integer @(Down Integer))
        it "is injective" (lawInjective @Integer @(Down Integer))
    describe "instance Injection (Down Integer) Integer" $ do
        it "is resolvable" (resolveInjection @(Down Integer) @Integer)
        it "is injective" (lawInjective @(Down Integer) @Integer)
    describe "instance Injection Integer (Product Integer)" $ do
        it "is resolvable" (resolveInjection @Integer @(Product Integer))
        it "is injective" (lawInjective @Integer @(Product Integer))
    describe "instance Injection (Product Integer) Integer" $ do
        it "is resolvable" (resolveInjection @(Product Integer) @Integer)
        it "is injective" (lawInjective @(Product Integer) @Integer)
    describe "instance Injection Integer (Sum Integer)" $ do
        it "is resolvable" (resolveInjection @Integer @(Sum Integer))
        it "is injective" (lawInjective @Integer @(Sum Integer))
    describe "instance Injection (Sum Integer) Integer" $ do
        it "is resolvable" (resolveInjection @(Sum Integer) @Integer)
        it "is injective" (lawInjective @(Sum Integer) @Integer)

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

instance Arbitrary a => Arbitrary (Down a) where
    arbitrary = Down <$> arbitrary
