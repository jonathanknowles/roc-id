{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ROC.ID.Number.Unchecked
  ( IdentityNumber (..)
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
import Data.Vector.Sized
  ( Vector )
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
import qualified Data.Vector.Sized as V
import qualified ROC.ID.Letter as Letter
import qualified ROC.ID.Digit as Digit
import qualified ROC.ID.Digit1289 as Digit1289

data IdentityNumber = IdentityNumber
  !Letter
  !Digit1289
  !(Vector 8 Digit)
  deriving stock (Eq, Ord, Show)

-- | Specifies the position of a character within a string.
--
newtype CharIndex = CharIndex Digit
  deriving stock (Read, Show)
  deriving newtype (Bounded, Enum, Eq, Num, Ord)

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

fromText :: Text -> Either FromTextError IdentityNumber
fromText text0 = do
    when (T.length text0 > 10) $ Left TextTooLong
    (text1, part0) <- parseLetter    text0
    (text2, part1) <- parseDigit1289 text1
    (_____, part2) <- parseDigits    text2
    pure (IdentityNumber part0 part1 part2)
  where
    parseLetter :: Parser Letter
    parseLetter text = do
      (char, remainder) <- guard TextTooShort $ T.uncons text
      letter <- guard (invalidChar letters D0) (Letter.fromChar char)
      pure (remainder, letter)

    parseDigit1289 :: Parser Digit1289
    parseDigit1289 text = do
      (char, remainder) <- guard TextTooShort $ T.uncons text
      digit1289 <- guard (invalidChar digits1289 D1) (Digit1289.fromChar char)
      pure (remainder, digit1289)

    parseDigits :: Parser (Vector 8 Digit)
    parseDigits text = do
        let (cs, remainder) = T.splitAt 8 text
        ds <- traverse parseIndexedDigit (zip [D2 ..] (T.unpack cs))
        vs <- guard TextTooShort (V.fromList @8 ds)
        pure (remainder, vs)
      where
        parseIndexedDigit (i, c) =
          guard (invalidChar digits i) (Digit.fromChar c)

    digits     = CharRange '0' '9'
    digits1289 = CharSet $ NESet.fromList $ '1' :| ['2', '8', '9']
    letters    = CharRange 'A' 'Z'

    invalidChar charSet index = InvalidChar (CharIndex index) charSet

toText :: IdentityNumber -> Text
toText (IdentityNumber u0 u1 u2) = t0 <> t1 <> t2
  where
    t0 = T.singleton (Letter.toChar u0)
    t1 = T.singleton (Digit1289.toChar u1)
    t2 = T.pack (Digit.toChar <$> V.toList u2)
