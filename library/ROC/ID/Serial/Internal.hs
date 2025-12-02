{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ROC.ID.Serial.Internal
  ( Serial (..)
  , fromTuple
  , toTuple
  , generate
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Vector.Sized
  ( Vector )
import GHC.Generics
  ( Generic )
import ROC.ID.Digit
  ( Digit )
import ROC.ID.Utilities
  ( randomBoundedEnum )

import qualified Data.Vector.Sized as V

-- | A 7-digit serial number, as found within an ROC identification number.
--
-- To generate a random 'Serial' number, use the 'generate' function.
--
newtype Serial = Serial (Vector 7 Digit)
  deriving (Eq, Generic, Ord, Show)

-- | Constructs a 'Serial' number from a tuple.
--
fromTuple :: d ~ Digit => (d, d, d, d, d, d, d) -> Serial
fromTuple = Serial . V.fromTuple

-- | Converts a 'Serial' number to a tuple.
--
toTuple :: d ~ Digit => Serial -> (d, d, d, d, d, d, d)
toTuple (Serial s) = (s!0, s!1, s!2, s!3, s!4, s!5, s!6)
  where
    (!) = V.index

-- | Generates a random 'Serial' number.
--
generate :: MonadRandom m => m Serial
generate = Serial <$> V.replicateM randomBoundedEnum
