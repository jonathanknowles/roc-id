{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ROC.ID
  ( Identity (..)

  -- * Parsing
  , fromText
  , FromTextError (..)
  , CharIndex (..)
  , CharSet (..)

  -- * Printing
  , toText

  -- * Verification
  , checksum

  -- * Generation
  , generate
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Text
  ( Text )
import GHC.Generics
  ( Generic )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Digit1289
  ( Digit1289 (..) )
import ROC.ID.Gender
  ( Gender (..) )
import ROC.ID.Location
  ( Location )
import ROC.ID.Nationality
  ( Nationality (..) )
import ROC.ID.Number
  ( IdentityNumber (..), FromTextError (..), CharIndex (..), CharSet (..) )
import ROC.ID.Serial
  ( Serial (Serial) )

import qualified Data.Text as T
import qualified ROC.ID.Gender as Gender
import qualified ROC.ID.Location as Location
import qualified ROC.ID.Location.Internal as Location
import qualified ROC.ID.Number as Number
import qualified ROC.ID.Serial as Serial
import qualified ROC.ID.Nationality as Nationality

-- | Represents a __valid__ 10-digit ROC national identification number
-- (中華民國身份證號碼) of the form __@A123456789@__.
--
-- By construction, invalid values are __not representable__ by this type.
--
-- An identification number encodes a person's 'Gender', the 'Location' in
-- which they first registered for an identification card, and a unique 'Serial'
-- number.
--
data Identity = Identity
  { gender :: !Gender
  -- ^ The gender of the person to whom this ID number belongs.
  , location :: !Location
  -- ^ The location in which the person first registered for an ID card.
  , nationality :: !Nationality
  -- ^ The nationality of the person to whom this ID number belongs.
  , serial :: !Serial
  -- ^ The serial number portion of this ID number.
  } deriving (Eq, Generic, Ord)

instance Read Identity where
  readsPrec _ s = do
    (token, remainder) <- lex s
    (unquotedString, "") <- reads token
    case fromText (T.pack unquotedString) of
      Right i -> pure (i, remainder)
      Left _ -> []

instance Show Identity where
  show = show . toText

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Attempt to parse an 'Identity' using the specified 'Text' as input.
--
-- The input must be of the form __@A123456789@__.
--
fromText :: Text -> Either FromTextError Identity
fromText t = fromNumber <$> Number.fromText t

--------------------------------------------------------------------------------
-- Printing
--------------------------------------------------------------------------------

-- | Print the specified 'Identity'.
--
-- The output is of the form __@A123456789@__.
--
toText :: Identity -> Text
toText = Number.toText . toNumber

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

-- | Calculate the checksum of the specified 'Identity'.
--
checksum :: Identity -> Digit
checksum = Number.checksum . toNumber

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- | Generate a random 'Identity'.
--
generate :: MonadRandom m => m Identity
generate =
  Identity
    <$> Gender.generate
    <*> Location.generate
    <*> Nationality.generate
    <*> Serial.generate

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

fromNumber :: IdentityNumber -> Identity
fromNumber (IdentityNumber c0 c1 c2) =
    Identity {gender, location, nationality, serial}
  where
    location = Location.fromLetter c0
    (gender, nationality) = case c1 of
      D1289_1 -> (  Male,    National)
      D1289_2 -> (Female,    National)
      D1289_8 -> (  Male, NonNational)
      D1289_9 -> (Female, NonNational)
    serial = Serial c2

toNumber :: Identity -> IdentityNumber
toNumber Identity {gender, location, nationality, serial} =
    IdentityNumber c0 c1 c2
  where
    c0 = Location.toLetter location
    c1 = case (gender, nationality) of
      (  Male,    National) -> D1289_1
      (Female,    National) -> D1289_2
      (  Male, NonNational) -> D1289_8
      (Female, NonNational) -> D1289_9
    c2 = case serial of Serial s -> s
