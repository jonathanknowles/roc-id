{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module ROC.ID.Raw
  ( RawID (..)
  , FromTextError (..)
  , CharIndex (..)
  , CharSet (..)
  , fromText
  , fromUnchecked
  , toText
  , toUnchecked
  , checksum
  , generate
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom )
import Data.Bifunctor
  ( Bifunctor (first) )
import Data.Text
  ( Text )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Digit1289
  ( Digit1289 (..) )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Raw.Unchecked
  ( CharIndex (..)
  , CharSet (..)
  , UncheckedRawID (UncheckedRawID)
  )
import ROC.ID.Utilities
  ( guard )

import qualified ROC.ID.Digit as Digit
import qualified ROC.ID.Digit1289 as Digit1289
import qualified ROC.ID.Letter as Letter
import qualified ROC.ID.Raw.Unchecked as U

data RawID = RawID
  { c0 :: !Letter
  , c1 :: !Digit1289
  , c2 :: !Digit
  , c3 :: !Digit
  , c4 :: !Digit
  , c5 :: !Digit
  , c6 :: !Digit
  , c7 :: !Digit
  , c8 :: !Digit
  }
  deriving (Eq, Ord, Show)

-- | Indicates an error that occurred while parsing an identification number.
--
data FromTextError

  = TextTooShort
  -- ^ Indicates that the input text is too short.

  | TextTooLong
  -- ^ Indicates that the input text is too long.

  | InvalidChar CharIndex CharSet
  -- ^ Indicates that the input text contains a character that is not allowed.
  --
  --   - `CharIndex` specifies the position of the offending character.
  --   - `CharSet` specifies the set of characters allowed at that position.

  | InvalidChecksum
  -- ^ Indicates that the parsed identification number has an invalid checksum.

  deriving (Eq, Ord, Show)

fromText :: Text -> Either FromTextError RawID
fromText text = do
    unchecked <- first fromUncheckedError $ U.fromText text
    guard InvalidChecksum $ fromUnchecked unchecked
  where
    fromUncheckedError :: U.FromTextError -> FromTextError
    fromUncheckedError = \case
      U.TextTooShort ->
        TextTooShort
      U.TextTooLong ->
        TextTooLong
      U.InvalidChar i r ->
        InvalidChar i r

toText :: RawID -> Text
toText = U.toText . toUnchecked

fromUnchecked :: UncheckedRawID -> Maybe RawID
fromUnchecked (UncheckedRawID u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
    | checksum i == u9 = Just i
    | otherwise = Nothing
  where
    i = RawID u0 u1 u2 u3 u4 u5 u6 u7 u8

toUnchecked :: RawID -> UncheckedRawID
toUnchecked i@(RawID u0 u1 u2 u3 u4 u5 u6 u7 u8) =
  UncheckedRawID u0 u1 u2 u3 u4 u5 u6 u7 u8 (checksum i)

checksum :: RawID -> Digit
checksum (RawID u0 (Digit1289.toDigit -> u1) u2 u3 u4 u5 u6 u7 u8) =
    negate $ sum $ zipWith (*)
      [ 1,  9,  8,  7,  6,  5,  4,  3,  2,  1]
      [a0, a1, u1, u2, u3, u4, u5, u6, u7, u8]
  where
    a0, a1 :: Digit
    (a0, a1) = case u0 of
      A -> (1, 0); N -> (2, 2)
      B -> (1, 1); O -> (3, 5)
      C -> (1, 2); P -> (2, 3)
      D -> (1, 3); Q -> (2, 4)
      E -> (1, 4); R -> (2, 5)
      F -> (1, 5); S -> (2, 6)
      G -> (1, 6); T -> (2, 7)
      H -> (1, 7); U -> (2, 8)
      I -> (3, 4); V -> (2, 9)
      J -> (1, 8); W -> (3, 2)
      K -> (1, 9); X -> (3, 0)
      L -> (2, 0); Y -> (3, 1)
      M -> (2, 1); Z -> (3, 3)

-- | Generates a random 'RawID'.
--
generate :: MonadRandom m => m RawID
generate =
  RawID
    <$> Letter.generate
    <*> Digit1289.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
