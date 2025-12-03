{-# LANGUAGE DeriveGeneric #-}

module ROC.ID.Digit
  ( Digit (..)
  , fromChar
  , toChar
  ) where

import GHC.Generics
  ( Generic )
import ROC.ID.Utilities
  ( maybeBoundedEnum )
import Text.Read
  ( readMaybe )

import Prelude hiding
  ( fromIntegral )

import qualified Prelude

-- | Represents a single decimal digit in the range @0@ to @9@.
--
data Digit
  = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Generic, Ord)

instance Num Digit where
  a + b = fromIntegral (fromEnum a + fromEnum b)
  a * b = fromIntegral (fromEnum a * fromEnum b)
  a - b = fromIntegral (fromEnum a - fromEnum b)
  abs a = a
  fromInteger = fromIntegral
  signum D0 = D0
  signum __ = D1

instance Show Digit where show = show . fromEnum

-- | Attempts to parse a 'Digit' from a character.
--
-- The 'Char' must be a decimal digit in the range @0@ to @9@.
--
fromChar :: Char -> Maybe Digit
fromChar c = readMaybe [c] >>= maybeBoundedEnum

-- | Converts a 'Digit' to a decimal digit character.
--
toChar :: Digit -> Char
toChar digit = case show digit of
  [c] -> c
  _ -> error "toChar"

-- | Creates a 'Digit' from an integral number (modulo 10).
--
fromIntegral :: Integral i => i -> Digit
fromIntegral i = toEnum (Prelude.fromIntegral (i `mod` 10))
