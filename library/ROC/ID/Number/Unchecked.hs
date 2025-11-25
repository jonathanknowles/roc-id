{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.List.NonEmpty
  ( NonEmpty ((:|)) )
import Data.Set.NonEmpty
  ( NESet )
import Data.Text
  ( Text )
import Data.Vector.Sized
  ( Vector )
import GHC.TypeLits
  ( KnownNat )
import ROC.ID.Digit
  ( Digit (..), Digit1289 )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Utilities
  ( guard )

import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import qualified Data.Vector.Sized as V
import qualified ROC.ID.Letter as Letter
import qualified ROC.ID.Digit as Digit

data IdentityNumber = IdentityNumber
  !Letter
  !Digit1289
  !(Vector 8 Digit)
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
    v <- guard invalidLength $ V.fromList @10 $ T.unpack text
    IdentityNumber
      <$> guard (invalidChar letters    D0) (Letter.fromChar    $ V.index v 0)
      <*> guard (invalidChar digits1289 D1) (Digit.fromChar1289 $ V.index v 1)
      <*> first
          (invalidChar digits . toEnum . (+ 2))
          (imapMay Digit.fromChar $ V.drop @2 v)
  where
    digits     = CharRange '0' '9'
    digits1289 = CharSet $ NESet.fromList $ '1' :| ['2', '8', '9']
    letters    = CharRange 'A' 'Z'

    invalidChar charSet index =
      InvalidChar (CharIndex index) charSet
    invalidLength =
      InvalidLength

toText :: IdentityNumber -> Text
toText (IdentityNumber u0 u1 u2) = t0 <> t1 <> t2
  where
    t0 = T.singleton (Letter.toChar u0)
    t1 = T.singleton (Digit.toChar1289 u1)
    t2 = T.pack (Digit.toChar <$> V.toList u2)

imapMay :: KnownNat n => (a -> Maybe b) -> Vector n a -> Either Int (Vector n b)
imapMay f = V.imapM (\i -> guard (fromIntegral i) . f)
