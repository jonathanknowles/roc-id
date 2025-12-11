{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ROC.ID.Unchecked
  ( UncheckedID (..)
  , FromTextError (..)
  , CharIndex (..)
  , CharSet (..)
  , fromText
  , toText
  )
  where

import Control.Monad
  ( when )
import Data.List.NonEmpty
  ( NonEmpty ((:|)) )
import Data.Set.NonEmpty
  ( NESet )
import Data.Text
  ( Text )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Digit1289
  ( Digit1289 )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Utilities
  ( guard )

import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import qualified ROC.ID.Digit as Digit
import qualified ROC.ID.Digit1289 as Digit1289
import qualified ROC.ID.Letter as Letter

data UncheckedID = UncheckedID
  { c0 :: !Letter
  , c1 :: !Digit1289
  , c2 :: !Digit
  , c3 :: !Digit
  , c4 :: !Digit
  , c5 :: !Digit
  , c6 :: !Digit
  , c7 :: !Digit
  , c8 :: !Digit
  , c9 :: !Digit
  }
  deriving stock (Eq, Ord, Show)

-- | Specifies the zero-based position of a character within a string.
--
newtype CharIndex = CharIndex Int
  deriving stock (Read, Show)
  deriving newtype (Enum, Eq, Num, Ord)

-- | Specifies a set of characters.
--
data CharSet
  = CharSet (NESet Char)
  -- ^ An explicit set of characters.
  | CharRange Char Char
  -- ^ An inclusive range of characters.
  deriving stock (Eq, Ord, Read, Show)

data FromTextError
  = TextTooShort
  | TextTooLong
  | InvalidChar CharIndex CharSet
  deriving stock (Eq, Ord, Read, Show)

type Parser a = Text -> Either FromTextError (Text, a)

fromText :: Text -> Either FromTextError UncheckedID
fromText text0 = do
    when (T.length text0 > 10) $ Left TextTooLong
    (text1,  c0                             ) <- parseLetter    text0
    (text2,  c1                             ) <- parseDigit1289 text1
    (_____, (c2, c3, c4, c5, c6, c7, c8, c9)) <- parseDigits    text2
    pure UncheckedID {c0, c1, c2, c3, c4, c5, c6, c7, c8, c9}
  where
    parseLetter :: Parser Letter
    parseLetter text = do
      (char, remainder) <- guard TextTooShort $ T.uncons text
      letter <- guard (InvalidChar 0 (CharRange 'A' 'Z')) (Letter.fromChar char)
      pure (remainder, letter)

    parseDigit1289 :: Parser Digit1289
    parseDigit1289 text = do
      (char, remainder) <- guard TextTooShort $ T.uncons text
      digit1289 <- guard
        (InvalidChar 1 (CharSet $ NESet.fromList $ '1' :| ['2', '8', '9']))
        (Digit1289.fromChar char)
      pure (remainder, digit1289)

    parseDigits :: d ~ Digit => Parser (d, d, d, d, d, d, d, d)
    parseDigits text = do
        let (chars, remainder) = T.splitAt 8 text
        digitList <- traverse parseIndexedDigit (zip [2 ..] $ T.unpack chars)
        digitTuple <- guard TextTooShort (listToTuple8 digitList)
        pure (remainder, digitTuple)
      where
        parseIndexedDigit (i, c) =
          guard (InvalidChar i (CharRange '0' '9')) (Digit.fromChar c)

toText :: UncheckedID -> Text
toText UncheckedID {c0, c1, c2, c3, c4, c5, c6, c7, c8, c9} =
  T.pack
    ( Letter.toChar c0
    : Digit1289.toChar c1
    : fmap Digit.toChar [c2, c3, c4, c5, c6, c7, c8, c9]
    )

listToTuple8 :: [a] -> Maybe (a, a, a, a, a, a, a, a)
listToTuple8 = \case
  (a0 : a1 : a2 : a3 : a4 : a5 : a6 : a7 : _) ->
    Just (a0, a1, a2, a3, a4, a5, a6, a7)
  _ ->
    Nothing
