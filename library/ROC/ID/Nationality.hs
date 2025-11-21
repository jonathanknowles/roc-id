module ROC.ID.Nationality
  ( Nationality (..)
  , generate
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import ROC.ID.Utilities
  ( randomBoundedEnum )

data Nationality = National | NonNational
  deriving (Bounded, Enum, Eq, Ord, Show)

generate :: MonadRandom m => m Nationality
generate = randomBoundedEnum
