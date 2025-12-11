{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Redundant bracket" -}

module Main where

import Data.Bifunctor
  ( Bifunctor (second) )
import Data.Char
  ( intToDigit )
import Data.List.NonEmpty
  ( NonEmpty ((:|)) )
import Data.Text
  ( Text )
import ROC.ID
  ( CharSet (CharSet), CharIndex (CharIndex), ID (..) )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Digit1289
  ( Digit1289 (..) )
import ROC.ID.Gender
  ( Gender (..) )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Location
  ( Location )
import ROC.ID.Nationality
  ( Nationality (..) )
import Test.Hspec
  ( describe, hspec, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
  ( Arbitrary (..)
  , NonEmptyList (..)
  , arbitraryBoundedEnum
  , choose
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

instance Arbitrary Digit where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Digit1289 where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Gender where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Letter where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary ID where
  arbitrary = ID.fromTuple <$> arbitrary
  shrink = shrinkMap ID.fromTuple ID.toTuple

instance Arbitrary Location where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

instance Arbitrary Nationality where
  arbitrary = arbitraryBoundedEnum
  shrink = shrinkBoundedEnum

main :: IO ()
main = hspec $ do

  describe "Class laws" $ do

    testLawsMany @Digit
        [ boundedEnumLaws
        , eqLaws
        , numLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLawsMany @Gender
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLawsMany @ID
        [ eqLaws
        , ordLaws
        , showLaws
        ]

    testLawsMany @Location
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

    testLawsMany @Nationality
        [ boundedEnumLaws
        , eqLaws
        , ordLaws
        , showLaws
        , showReadLaws
        ]

  describe "ID.fromText" $ do

    it "successfully parses known-valid identification numbers" $
      do
        let i = ID.fromText "A123456789"
        i `shouldBe` Right (ID A D1289_1 2 3 4 5 6 7 8)
        fmap ID.checksum i `shouldBe` Right 9

    it "successfully parses valid identification numbers" $
      property $ \(i :: ID) ->
        ID.fromText (ID.toText i) `shouldBe` Right i

    it "does not parse identification numbers that are too short" $
      property $ \(i :: ID) n -> do
        let newLength = n `mod` 10
        let invalidID = T.take newLength $ ID.toText i
        ID.fromText invalidID `shouldBe` Left ID.TextTooShort

    it "does not parse identification numbers that are too long" $
      property $ \(i :: ID) (NonEmpty s) -> do
        let invalidID = ID.toText i <> T.pack s
        ID.fromText invalidID `shouldBe` Left ID.TextTooLong

    it "does not parse identification numbers with invalid location codes" $
      property $ \(i :: ID) (c :: Int) -> do
        let invalidLocationCode = intToDigit $ c `mod` 10
        let invalidID = replaceCharAt 0 invalidLocationCode $ ID.toText i
        ID.fromText invalidID `shouldBe`
          Left (ID.InvalidChar 0 (ID.CharRange 'A' 'Z'))

    it "does not parse identification numbers with invalid initial digits" $
      property $ \(i :: ID) ->
        forAll (elements ['0', '3', '4', '5', '6', '7']) $ \initialDigit -> do
          let invalidID = replaceCharAt 1 initialDigit (ID.toText i)
          let expectedError =
                ID.InvalidChar 1
                  (CharSet $ NESet.fromList $ '1' :| ['2', '8', '9'])
          ID.fromText invalidID `shouldBe` Left expectedError

    it "does not parse identification numbers with invalid checksums" $
      property $ \(i :: ID) (c :: Int) -> do
        let invalidChecksum = intToDigit $
              ((c `mod` 9) + fromEnum (ID.checksum i) + 1) `mod` 10
        let invalidID =
              T.take 9 (ID.toText i) <> T.pack [invalidChecksum]
        ID.fromText invalidID `shouldBe` Left ID.InvalidChecksum

    it "reports invalid characters even when input is too short" $
      property $ \(identity :: ID) ->
      forAll (choose (1, 9)) $ \truncatedLength ->
      forAll (choose (0, truncatedLength - 1)) $ \invalidCharIndex -> do
        let textTruncated = T.take truncatedLength (ID.toText identity)
        let textInvalid = replaceCharAt invalidCharIndex 'x' textTruncated
        ID.fromText textInvalid `shouldSatisfy` \case
          Left (ID.InvalidChar (CharIndex i) _) | i == invalidCharIndex -> True
          _ -> False

    it "does not report invalid characters if input is too long" $
      property $ \(identity :: ID) (NonEmpty trailingExcess) ->
      forAll (choose (0, 9)) $ \invalidCharIndex -> do
        let textInvalid =
              replaceCharAt invalidCharIndex 'x' (ID.toText identity)
              <>
              T.pack trailingExcess
        ID.fromText textInvalid `shouldBe` Left ID.TextTooLong

-- | Replaces a character at a specific position.
--
replaceCharAt :: Int -> Char -> Text -> Text
replaceCharAt i c t
    | i < 0 || i >= T.length t = error "replaceCharAt: invalid index"
    | otherwise = prefix <> T.singleton c <> suffix
  where
    (prefix, suffix) = second (T.drop 1) (T.splitAt i t)
