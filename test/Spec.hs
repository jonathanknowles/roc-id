{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}

module Main where

import Data.Char
  ( intToDigit )
import ROC.ID
  ( Identity (Identity), ParseError (..), identityChecksum, parseIdentity )
import ROC.ID.Digit
  ( Digit )
import ROC.ID.Gender
  ( Gender )
import ROC.ID.Location
  ( Location )
import ROC.ID.Serial
  ( Serial (..) )
import Test.Hspec
  ( describe, hspec, it, shouldBe )
import Test.QuickCheck
  ( Arbitrary (..)
  , NonEmptyList (..)
  , applyArbitrary3
  , arbitraryBoundedEnum
  , property
  , shrinkBoundedEnum
  , shrinkMap
  )
import Test.QuickCheck.Classes
  ( eqLaws, ordLaws, showLaws, showReadLaws )
import Test.QuickCheck.Classes.Hspec
  ( testLawsMany )

import qualified Data.Text as T
import qualified Data.Vector.Sized as V

instance Arbitrary Digit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Gender where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Identity where
  arbitrary = applyArbitrary3 Identity
  shrink = shrinkMap unTuple toTuple
    where
      toTuple (Identity g l s) = (g, l, s)
      unTuple (g, l, s) = (Identity g l s)

instance Arbitrary Location where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Serial where
  arbitrary = Serial . V.fromTuple <$> arbitrary
  shrink (Serial v) = Serial <$> traverse shrink v

main :: IO ()
main = hspec $ do

  describe "Class laws" $ do

    testLawsMany @Identity
        [ eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

  describe "parseIdentity" $ do

    it "successfully parses valid identification numbers" $
      property $ \(i :: Identity) ->
        parseIdentity (T.pack $ showIdentityUnquoted i) `shouldBe` Right i

    it "does not parse identification numbers that are too short" $
      property $ \(i :: Identity) n -> do
        let newLength = n `mod` 10
        let invalidIdentity = T.take newLength $ T.pack $ showIdentityUnquoted i
        parseIdentity invalidIdentity `shouldBe` Left InvalidLength

    it "does not parse identification numbers that are too long" $
      property $ \(i :: Identity) (NonEmpty s) -> do
        let invalidIdentity = T.pack (showIdentityUnquoted i) <> T.pack s
        parseIdentity invalidIdentity `shouldBe` Left InvalidLength

    it "does not parse identification numbers with invalid gender codes" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidGenderCode = intToDigit $ ((c `mod` 8) + 3) `mod` 10
        let invalidIdentity =
              T.take 1 (T.pack $ showIdentityUnquoted i) <>
              T.pack [invalidGenderCode] <>
              T.drop 2 (T.pack $ showIdentityUnquoted i)
        parseIdentity invalidIdentity `shouldBe` Left InvalidGender

    it "does not parse identification numbers with invalid location codes" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidLocationCode = intToDigit $ c `mod` 10
        let invalidIdentity =
              T.cons invalidLocationCode
                $ T.drop 1
                $ T.pack
                $ showIdentityUnquoted i
        parseIdentity invalidIdentity `shouldBe` Left InvalidLocation

    it "does not parse identification numbers with invalid checksums" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidChecksum = intToDigit $
              ((c `mod` 9) + fromEnum (identityChecksum i) + 1) `mod` 10
        let invalidIdentity =
              T.take 9 (T.pack $ showIdentityUnquoted i) <>
              T.pack [invalidChecksum]
        parseIdentity invalidIdentity `shouldBe` Left InvalidChecksum

showIdentityUnquoted :: Identity -> String
showIdentityUnquoted i = read $ show i
