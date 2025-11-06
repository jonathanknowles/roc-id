{-# LANGUAGE ScopedTypeVariables #-}

module ROC.ID.Utilities where

import Control.Monad.Random.Class
  ( MonadRandom (..) )

guard :: x -> Maybe y -> Either x y
guard x = maybe (Left x) Right

maybeBoundedEnum :: forall a. (Bounded a, Enum a) => Int -> Maybe a
maybeBoundedEnum i
  | i < fromEnum (minBound :: a) = Nothing
  | i > fromEnum (maxBound :: a) = Nothing
  | otherwise                    = pure $ toEnum i

randomBoundedEnum :: forall m a. (MonadRandom m, Bounded a, Enum a) => m a
randomBoundedEnum =
  toEnum <$> getRandomR (fromEnum (minBound :: a), fromEnum (maxBound :: a))
