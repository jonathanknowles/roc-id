{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}

module Main where

import Data.Char
  ( intToDigit )
import Data.List.NonEmpty
  ( NonEmpty ((:|)) )
import ROC.ID
  ( Identity (Identity), CharSet (CharSet) )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Gender
  ( Gender )
import ROC.ID.Location
  ( Location )
import ROC.ID.Nationality
  ( Nationality )
import ROC.ID.Serial
  ( Serial )
import Test.Hspec
  ( describe, hspec, it, shouldBe )
import Test.QuickCheck
  ( Arbitrary (..)
  , NonEmptyList (..)
  , applyArbitrary4
  , arbitraryBoundedEnum
  , elements
  , forAll
  , property
  , shrinkBoundedEnum
  , shrinkMap
  )
import Test.QuickCheck.Classes
  ( boundedEnumLaws, eqLaws, numLaws, ordLaws, showLaws, showReadLaws )
import Test.QuickCheck.Classes.Hspec
  ( testLawsMany )

import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import qualified ROC.ID as ID
import qualified ROC.ID.Serial as Serial

instance Arbitrary Digit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Gender where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Identity where
  arbitrary = applyArbitrary4 Identity
  shrink = shrinkMap unTuple toTuple
    where
      toTuple (Identity g l n s) = (g, l, n, s)
      unTuple (g, l, n, s) = (Identity g l n s)

instance Arbitrary Location where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Nationality where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Serial where
  arbitrary = Serial.fromTuple <$> arbitrary
  shrink = shrinkMap Serial.fromTuple Serial.toTuple

main :: IO ()
main = hspec $ do

  describe "Class laws" $ do

    testLawsMany @Digit
        [ boundedEnumLaws
        , eqLaws
        , numLaws
        , ordLaws
        , showLaws
        ]

    testLawsMany @Identity
        [ eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

  describe "ID.fromText" $ do

    it "successfully parses valid identification numbers" $
      property $ \(i :: Identity) ->
        ID.fromText (ID.toText i) `shouldBe` Right i

    it "does not parse identification numbers that are too short" $
      property $ \(i :: Identity) n -> do
        let newLength = n `mod` 10
        let invalidIdentity = T.take newLength $ ID.toText i
        ID.fromText invalidIdentity `shouldBe` Left ID.InvalidLength

    it "does not parse identification numbers that are too long" $
      property $ \(i :: Identity) (NonEmpty s) -> do
        let invalidIdentity = ID.toText i <> T.pack s
        ID.fromText invalidIdentity `shouldBe` Left ID.InvalidLength

    it "does not parse identification numbers with invalid location codes" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidLocationCode = intToDigit $ c `mod` 10
        let invalidIdentity =
              T.cons invalidLocationCode $ T.drop 1 $ ID.toText i
        ID.fromText invalidIdentity `shouldBe`
          Left (ID.InvalidChar (ID.CharIndex D0) (ID.CharRange 'A' 'Z'))

    it "does not parse identification numbers with invalid initial digits" $
      property $ \(i :: Identity) ->
        forAll (elements ['0', '3', '4', '5', '6', '7']) $ \initialDigit -> do
          let invalidIdentity =
                T.take 1 (ID.toText i) <>
                T.pack [initialDigit] <>
                T.drop 2 (ID.toText i)
          let expectedError =
                ID.InvalidChar
                  (ID.CharIndex D1)
                  (CharSet $ NESet.fromList $ '1' :| ['2', '8', '9'])
          ID.fromText invalidIdentity `shouldBe` Left expectedError

    it "does not parse identification numbers with invalid checksums" $
      property $ \(i :: Identity) (c :: Int) -> do
        let invalidChecksum = intToDigit $
              ((c `mod` 9) + fromEnum (ID.checksum i) + 1) `mod` 10
        let invalidIdentity =
              T.take 9 (ID.toText i) <> T.pack [invalidChecksum]
        ID.fromText invalidIdentity `shouldBe` Left ID.InvalidChecksum
