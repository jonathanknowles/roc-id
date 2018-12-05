{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ROC.ID.Internal

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import qualified Data.Vector.Sized as V
import qualified Data.Text         as T

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
main = hspec $

  describe "parseIdentity" $

    it "successfully parses valid identification numbers" $
      property $ \(i :: Identity) ->
        parseIdentity (T.pack $ show i) `shouldBe` Right i
