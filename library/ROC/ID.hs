{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ROC.ID
  ( ID (..)

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
import ROC.ID.Raw
  ( RawID (..), FromTextError (..), CharIndex (..), CharSet (..) )
import ROC.ID.Serial.Internal
  ( Serial )

import qualified ROC.ID.Gender as Gender
import qualified ROC.ID.Location as Location
import qualified ROC.ID.Raw as Raw
import qualified ROC.ID.Serial as Serial
import qualified ROC.ID.Nationality as Nationality

-- | Represents a __valid__ 10-digit ROC (Taiwan) Uniform Identification Number
-- (中華民國統一證號) of the form __@A123456789@__.
--
-- By construction, invalid identification numbers are __not representable__ by
-- this type.
--
data ID = ID
  { gender :: !Gender
  -- ^ The gender of the person to whom this ID number belongs.
  , location :: !Location
  -- ^ The location in which the person first registered for an ID card.
  , nationality :: !Nationality
  -- ^ The nationality of the person to whom this ID number belongs.
  , serial :: !Serial
  -- ^ The serial number portion of this ID number.
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Attempts to parse an 'ID' using the specified 'Text' as input.
--
-- The input must be exactly 10 characters in length and of the form
-- __@A123456789@__.
--
-- More precisely, the input must match the regular expression
-- __@^[A-Z][1289][0-9]{8}$@__.
--
fromText :: Text -> Either FromTextError ID
fromText t = fromRaw <$> Raw.fromText t

--------------------------------------------------------------------------------
-- Printing
--------------------------------------------------------------------------------

-- | Prints the specified 'ID'.
--
-- The output is of the form __@A123456789@__.
--
toText :: ID -> Text
toText = Raw.toText . toRaw

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

-- | Calculates the checksum of the specified 'ID'.
--
checksum :: ID -> Digit
checksum = Raw.checksum . toRaw

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- | Generates a random 'ID'.
--
generate :: MonadRandom m => m ID
generate =
  ID
    <$> Gender.generate
    <*> Location.generate
    <*> Nationality.generate
    <*> Serial.generate

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

fromRaw :: RawID -> ID
fromRaw (RawID c0 c1 c2 c3 c4 c5 c6 c7 c8) =
    ID {gender, location, nationality, serial}
  where
    location = Location.fromLetter c0
    (gender, nationality) = case c1 of
      D1289_1 -> (  Male,    National)
      D1289_2 -> (Female,    National)
      D1289_8 -> (  Male, NonNational)
      D1289_9 -> (Female, NonNational)
    serial = Serial.fromTuple (c2, c3, c4, c5, c6, c7, c8)

toRaw :: ID -> RawID
toRaw ID {gender, location, nationality, serial} =
    RawID c0 c1 c2 c3 c4 c5 c6 c7 c8
  where
    c0 = Location.toLetter location
    c1 = case (gender, nationality) of
      (  Male,    National) -> D1289_1
      (Female,    National) -> D1289_2
      (  Male, NonNational) -> D1289_8
      (Female, NonNational) -> D1289_9
    (c2, c3, c4, c5, c6, c7, c8) = Serial.toTuple serial
