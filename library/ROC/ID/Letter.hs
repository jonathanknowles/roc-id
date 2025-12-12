{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ROC.ID.Letter
  ( Letter (..)
  , fromChar
  , toChar
  , generate
  , FromChar
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import GHC.Generics
  ( Generic )
import GHC.TypeError
  ( ErrorMessage (Text), TypeError )
import ROC.ID.Utilities
  ( randomBoundedEnum )
import Text.Read
  ( readMaybe )

-- | Represents a letter in the latin alphabet.
--
data Letter
  = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Attempts to parse a 'Letter' from a character.
--
-- Returns 'Nothing' if the specified character is not an uppercase alphabetic
-- character.
--
fromChar :: Char -> Maybe Letter
fromChar c = readMaybe [c]

-- | Converts the specified 'Letter' to a character.
--
toChar :: Letter -> Char
toChar letter = case show letter of
  [c] -> c
  _ -> error "toChar"

-- | Generates a random 'Letter'.
--
generate :: MonadRandom m => m Letter
generate = randomBoundedEnum

type family FromChar (c :: Char) :: Letter where
  FromChar 'A' = A; FromChar 'N' = N
  FromChar 'B' = B; FromChar 'O' = O
  FromChar 'C' = C; FromChar 'P' = P
  FromChar 'D' = D; FromChar 'Q' = Q
  FromChar 'E' = E; FromChar 'R' = R
  FromChar 'F' = F; FromChar 'S' = S
  FromChar 'G' = G; FromChar 'T' = T
  FromChar 'H' = H; FromChar 'U' = U
  FromChar 'I' = I; FromChar 'V' = V
  FromChar 'J' = J; FromChar 'W' = W
  FromChar 'K' = K; FromChar 'X' = X
  FromChar 'L' = L; FromChar 'Y' = Y
  FromChar 'M' = M; FromChar 'Z' = Z
  FromChar _ = TypeError (Text "Expected uppercase letter.")
