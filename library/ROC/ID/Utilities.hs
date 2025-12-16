{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ROC.ID.Utilities where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import GHC.TypeLits (Symbol, UnconsSymbol)

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

type family FromJust e a where
  FromJust e  Nothing = e
  FromJust _ (Just a) = a

type family Fst (t :: (a, a)) :: a where
  Fst '(x, _) = x

type family Snd (t :: (a, a)) :: a where
  Snd '(_, y) = y

type family
    SymbolToCharList (s :: Symbol) :: [Char]
  where
    SymbolToCharList s = MaybeCharSymbolToCharList (UnconsSymbol s)

type family
  MaybeCharSymbolToCharList
    (m :: Maybe (Char, Symbol)) :: [Char]
  where
    MaybeCharSymbolToCharList ('Just '(c, rest)) = c ': SymbolToCharList rest
    MaybeCharSymbolToCharList 'Nothing = '[]
