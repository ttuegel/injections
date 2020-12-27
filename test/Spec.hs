module Main (main) where

import Injection

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "inject :: a -> TestInjection a" $ do
        it "wraps the input value" $ do
            unTestInjection (inject ()) `shouldBe` ()
        it "given different inputs, the output is different" $ do
            (inject @Integer @(TestInjection Integer) 1 == inject @Integer 2) `shouldBe` False

newtype TestInjection a = TestInjection { unTestInjection :: a }
    deriving stock (Eq)

instance Injection a (TestInjection a) where
    inject = TestInjection
