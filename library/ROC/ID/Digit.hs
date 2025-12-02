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

-- | Represents a single decimal digit in the range @0@ to @9@.
--
data Digit
  = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Generic, Ord)

instance Show Digit where show = show . fromEnum

fromChar :: Char -> Maybe Digit
fromChar c = readMaybe [c] >>= maybeBoundedEnum

toChar :: Digit -> Char
toChar digit = case show digit of
  [c] -> c
  _ -> error "toChar"
