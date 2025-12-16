{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ROC.ID.Digit
  ( Digit (..)
  , fromChar
  , toChar
  , generate
  , FromChar
  , FromNat
  , ToNat
  ) where

import Control.Monad.Random
  ( MonadRandom )
import GHC.Generics
  ( Generic )
import GHC.TypeError
  ( ErrorMessage (Text), TypeError )
import GHC.TypeNats
  ( Nat )
import ROC.ID.Utilities
  ( maybeBoundedEnum, randomBoundedEnum )
import Text.Read
  ( Read (readPrec), readMaybe )

import Prelude hiding
  ( fromIntegral )

import qualified Prelude

-- | Represents a single decimal digit in the range @0@ to @9@.
--
data Digit
  = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Generic, Ord)

-- | Arithmetic modulo 10.
--
instance Num Digit where
  a + b = fromIntegral (fromEnum a + fromEnum b)
  a * b = fromIntegral (fromEnum a * fromEnum b)
  a - b = fromIntegral (fromEnum a - fromEnum b)
  abs a = a
  fromInteger = fromIntegral
  signum D0 = D0
  signum __ = D1

instance Read Digit where readPrec = fromInteger <$> readPrec

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

-- | Generates a random digit.
--
generate :: MonadRandom m => m Digit
generate = randomBoundedEnum

type RangeError =
  TypeError (Text "Digit must be in the range [0 .. 9].")

type family FromChar (c :: Char) :: Digit where
  FromChar '0' = D0
  FromChar '1' = D1
  FromChar '2' = D2
  FromChar '3' = D3
  FromChar '4' = D4
  FromChar '5' = D5
  FromChar '6' = D6
  FromChar '7' = D7
  FromChar '8' = D8
  FromChar '9' = D9
  FromChar _ = RangeError

type family FromNat (n :: Nat) :: Digit where
  FromNat 0 = D0
  FromNat 1 = D1
  FromNat 2 = D2
  FromNat 3 = D3
  FromNat 4 = D4
  FromNat 5 = D5
  FromNat 6 = D6
  FromNat 7 = D7
  FromNat 8 = D8
  FromNat 9 = D9
  FromNat _ = RangeError

type family ToNat (d :: Digit) :: Nat where
  ToNat D0 = 0
  ToNat D1 = 1
  ToNat D2 = 2
  ToNat D3 = 3
  ToNat D4 = 4
  ToNat D5 = 5
  ToNat D6 = 6
  ToNat D7 = 7
  ToNat D8 = 8
  ToNat D9 = 9
