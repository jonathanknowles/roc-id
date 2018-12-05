module Main where

import ROC.ID

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import qualified Data.Vector.Sized as V

instance Arbitrary Digit where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Gender where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Identity where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Location where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Serial where
  arbitrary = Serial . V.fromTuple <$> arbitrary
  shrink (Serial v) = Serial <$> traverse shrink v

main :: IO ()
main = putStrLn "Test suite not yet implemented"
