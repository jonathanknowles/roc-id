{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ROC.ID.Digit1289
  ( Digit1289 (..)
  , fromChar
  , toChar
  , toDigit
  , generate
  ) where

import Control.Monad.Random
  ( MonadRandom )
import GHC.Generics
  ( Generic )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Utilities
  ( randomBoundedEnum )

-- | Represents a single decimal digit from the set {@1@, @2@, @8@, @9@}.
--
data Digit1289
  = D1289_1 | D1289_2 | D1289_8 | D1289_9
  deriving (Bounded, Enum, Eq, Generic, Ord)

instance Show Digit1289 where
  show = (: []) . toChar

-- | Attempts to parse a 'Digit1289' from a character.
--
-- Only the characters @'1'@, @'2'@, @'8'@, and @'9'@ are accepted.
--
fromChar :: Char -> Maybe Digit1289
fromChar = \case
  '1' -> Just D1289_1
  '2' -> Just D1289_2
  '8' -> Just D1289_8
  '9' -> Just D1289_9
  _   -> Nothing

-- | Converts a 'Digit1289' to a decimal digit character.
--
toChar :: Digit1289 -> Char
toChar = \case
  D1289_1 -> '1'
  D1289_2 -> '2'
  D1289_8 -> '8'
  D1289_9 -> '9'

-- | Converts a 'Digit1289' to an ordinary 'Digit'.
--
toDigit :: Digit1289 -> Digit
toDigit = \case
  D1289_1 -> D1
  D1289_2 -> D2
  D1289_8 -> D8
  D1289_9 -> D9

-- | Generates a random 'Digit1289'.
--
generate :: MonadRandom m => m Digit1289
generate = randomBoundedEnum
