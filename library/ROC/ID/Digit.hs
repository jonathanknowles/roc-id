{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ROC.ID.Digit
  ( Digit (..)
  , Digit12 (..)
  , fromChar
  , fromChar12
  , fromDigit12
  , toChar
  , toChar12
  ) where

import GHC.Generics
  ( Generic )
import ROC.ID.Utilities
  ( maybeBoundedEnum )
import Text.Read
  ( readMaybe )

-- | Represents a single decimal digit in the range 0 to 9.
--
data Digit
  = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Generic, Ord)

data Digit12
  = D12_1 | D12_2
  deriving (Bounded, Enum, Eq, Generic, Ord)

instance Show Digit where show = show . fromEnum

instance Show Digit12 where
  show = (: []) . toChar12

fromChar :: Char -> Maybe Digit
fromChar c = readMaybe [c] >>= maybeBoundedEnum

fromChar12 :: Char -> Maybe Digit12
fromChar12 = \case
  '1' -> Just D12_1
  '2' -> Just D12_2
  _   -> Nothing

toChar :: Digit -> Char
toChar digit = case show digit of
  [c] -> c
  _ -> error "toChar"

toChar12 :: Digit12 -> Char
toChar12 = \case
  D12_1 -> '1'
  D12_2 -> '2'

fromDigit12 :: Digit12 -> Digit
fromDigit12 = \case
  D12_1 -> D1
  D12_2 -> D2
