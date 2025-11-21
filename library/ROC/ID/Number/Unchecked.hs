{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module ROC.ID.Number.Unchecked
  ( IdentityNumber (..)
  , FromTextError (..)
  , CharIndex (..)
  , CharSet (..)
  , fromText
  , toText
  )
  where

import Data.Bifunctor
  ( Bifunctor (first) )
import Data.Set.NonEmpty
  ( NESet )
import Data.Text
  ( Text )
import Data.Vector.Sized
  ( Vector )
import GHC.TypeLits
  ( KnownNat )
import ROC.ID.Digit
  ( Digit (..), Digit12 )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Utilities
  ( guard )

import qualified Data.Text as T
import qualified ROC.ID.Letter as Letter
import qualified ROC.ID.Digit as Digit
import qualified Data.Vector.Sized as V

data IdentityNumber = IdentityNumber
  { u0 :: !Letter
  , u1 :: !Digit12
  , u2 :: !(Vector 8 Digit)
  }
  deriving (Eq, Ord, Show)

newtype CharIndex = CharIndex Digit
  deriving (Bounded, Enum, Eq, Ord, Show)

data CharSet
  = CharSet (NESet Char)
  | CharRange Char Char
  deriving (Eq, Ord, Show)

data FromTextError
  = InvalidLength
  | InvalidChar CharIndex CharSet
  deriving (Eq, Ord, Show)

fromText :: Text -> Either FromTextError IdentityNumber
fromText text = do
    v  <- guard invalidLength $ V.fromList @10 $ T.unpack text
    IdentityNumber
      <$> guard (invalidChar ('A', 'Z') D0) (Letter.fromChar $ V.index v 0)
      <*> guard (invalidChar ('1', '2') D1) (Digit.fromChar12 $ V.index v 1)
      <*> first
          (invalidChar ('0', '9') . toEnum . (+ 2))
          (imapMay Digit.fromChar $ V.drop @2 v)
  where
    invalidChar (lo, hi) index =
      InvalidChar (CharIndex index) (CharRange lo hi)
    invalidLength =
      InvalidLength

toText :: IdentityNumber -> Text
toText IdentityNumber {u0, u1, u2} = t0 <> t1 <> t2
  where
    t0 = T.singleton (Letter.toChar u0)
    t1 = T.singleton (Digit.toChar12 u1)
    t2 = T.pack (Digit.toChar <$> V.toList u2)

imapMay :: KnownNat n => (a -> Maybe b) -> Vector n a -> Either Int (Vector n b)
imapMay f = V.imapM (\i -> guard (fromIntegral i) . f)
