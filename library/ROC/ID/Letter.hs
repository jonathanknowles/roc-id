{-# LANGUAGE DeriveGeneric #-}

module ROC.ID.Letter
  ( Letter (..)
  , fromChar
  , toChar
  , generate
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import GHC.Generics
  ( Generic )
import ROC.ID.Utilities
  ( randomBoundedEnum )
import Text.Read
  ( readMaybe )

data Letter
  = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

fromChar :: Char -> Maybe Letter
fromChar c = readMaybe [c]

toChar :: Letter -> Char
toChar letter = case show letter of
  [c] -> c
  _ -> error "toChar"

generate :: MonadRandom m => m Letter
generate = randomBoundedEnum
