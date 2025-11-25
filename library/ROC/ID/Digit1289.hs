{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ROC.ID.Digit1289
  ( Digit1289 (..)
  , fromChar
  , toChar
  , toDigit
  ) where

import ROC.ID.Digit
  ( Digit (..) )
import GHC.Generics
  ( Generic )

data Digit1289
  = D1289_1 | D1289_2 | D1289_8 | D1289_9
  deriving (Bounded, Enum, Eq, Generic, Ord)

instance Show Digit1289 where
  show = (: []) . toChar

fromChar :: Char -> Maybe Digit1289
fromChar = \case
  '1' -> Just D1289_1
  '2' -> Just D1289_2
  '8' -> Just D1289_8
  '9' -> Just D1289_9
  _   -> Nothing

toChar :: Digit1289 -> Char
toChar = \case
  D1289_1 -> '1'
  D1289_2 -> '2'
  D1289_8 -> '8'
  D1289_9 -> '9'

toDigit :: Digit1289 -> Digit
toDigit = \case
  D1289_1 -> D1
  D1289_2 -> D2
  D1289_8 -> D8
  D1289_9 -> D9
