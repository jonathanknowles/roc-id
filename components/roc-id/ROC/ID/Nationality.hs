{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module ROC.ID.Nationality
  ( Nationality (..)
  , generate
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import GHC.Generics
  ( Generic )
import ROC.ID.Utilities
  ( randomBoundedEnum )

-- | Specifies a person's nationality.
--
data Nationality = National | NonNational
  deriving stock (Bounded, Enum, Eq, Ord, Generic, Read, Show)

-- | Generates a random 'Nationality'.
--
generate :: MonadRandom m => m Nationality
generate = randomBoundedEnum
