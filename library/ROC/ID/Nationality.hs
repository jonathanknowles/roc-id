module ROC.ID.Nationality
  ( Nationality (..)
  , generate
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import ROC.ID.Utilities
  ( randomBoundedEnum )

-- | Specifies a person's nationality.
--
data Nationality = National | NonNational
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Generates a random 'Nationality'.
--
generate :: MonadRandom m => m Nationality
generate = randomBoundedEnum
