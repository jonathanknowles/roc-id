{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ROC.ID
  (
  -- * Type
    ID (..)

  -- * Construction
  , fromSymbol
  , fromText
  , FromTextError (..)
  , CharIndex (..)
  , CharSet (..)
  , fromTuple

  -- * Conversion
  , toText
  , toTuple

  -- * Verification
  , checksumDigit

  -- * Generation
  , generate

  -- * Inspection
  , getGender
  , getLocation
  , getNationality

  -- * Modification
  , setGender
  , setLocation
  , setNationality
  )
  where

import Control.Monad.Random.Class
  ( MonadRandom )
import Data.Bifunctor
  ( Bifunctor (first) )
import Data.Proxy
  ( Proxy (Proxy) )
import Data.Text
  ( Text )
import GHC.TypeLits
  ( Symbol, symbolVal )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Digit1289
  ( Digit1289 (..) )
import ROC.ID.Gender
  ( Gender (..) )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Location
  ( Location )
import ROC.ID.Nationality
  ( Nationality (..) )
import ROC.ID.Unchecked
  ( CharIndex (..)
  , CharSet (..)
  , UncheckedID (UncheckedID)
  , ValidID
  )
import ROC.ID.Utilities
  ( guard )

import qualified Data.Text as T
import qualified ROC.ID.Digit as Digit
import qualified ROC.ID.Digit1289 as Digit1289
import qualified ROC.ID.Letter as Letter
import qualified ROC.ID.Location as Location
import qualified ROC.ID.Unchecked as U

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Represents a __valid__ 10-digit ROC (Taiwan) Uniform Identification Number
-- (中華民國統一證號) of the form __@A123456789@__.
--
-- By construction, invalid identification numbers are __not representable__ by
-- this type.
--
-- To calculate the checksum digit of an 'ID', use the 'checksum' function.
--
data ID = ID
  { c0 :: !Letter
  , c1 :: !Digit1289
  , c2 :: !Digit
  , c3 :: !Digit
  , c4 :: !Digit
  , c5 :: !Digit
  , c6 :: !Digit
  , c7 :: !Digit
  , c8 :: !Digit
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Constructs an 'ID' from a type-level symbol.
--
fromSymbol :: forall (s :: Symbol). ValidID s => ID
fromSymbol = unsafeFromText $ T.pack $ symbolVal $ Proxy @s

-- | Attempts to construct an 'ID' from 'Text'.
--
-- The input must be exactly 10 characters in length and of the form
-- __@A123456789@__.
--
-- More precisely, the input must match the regular expression
-- __@^[A-Z][1289][0-9]{8}$@__.
--
-- This function satisfies the following law:
--
-- @
-- 'fromText' ('toText' i) '==' 'Right' i
-- @
--
fromText :: Text -> Either FromTextError ID
fromText text = do
    unchecked <- first fromUncheckedError $ U.fromText text
    guard InvalidChecksum $ fromUnchecked unchecked
  where
    fromUncheckedError :: U.FromTextError -> FromTextError
    fromUncheckedError = \case
      U.TextTooShort ->
        TextTooShort
      U.TextTooLong ->
        TextTooLong
      U.InvalidChar i r ->
        InvalidChar i r

unsafeFromText :: Text -> ID
unsafeFromText t =
  case fromText t of
    Left _ -> error "unsafeFromText"
    Right i -> i

-- | Indicates an error that occurred while constructing an 'ID' from 'Text'.
--
data FromTextError

  = TextTooShort
  -- ^ Indicates that the input text is too short.

  | TextTooLong
  -- ^ Indicates that the input text is too long.

  | InvalidChar CharIndex CharSet
  -- ^ Indicates that the input text contains a character that is not allowed.
  --
  --   - `CharIndex` specifies the position of the offending character.
  --   - `CharSet` specifies the set of characters allowed at that position.

  | InvalidChecksum
  -- ^ Indicates that the parsed identification number has an invalid checksum.

  deriving (Eq, Ord, Show)

-- | Constructs an 'ID' from a tuple.
--
-- This function satisfies the following laws:
--
-- @
-- 'fromTuple' ('toTuple' i) '==' i
-- @
-- @
-- 'toTuple' ('fromTuple' t) '==' t
-- @
--
fromTuple :: Digit ~ d => (Letter, Digit1289, d, d, d, d, d, d, d) -> ID
fromTuple (c0, c1, c2, c3, c4, c5, c6, c7, c8) =
  ID c0 c1 c2 c3 c4 c5 c6 c7 c8

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- | Converts an 'ID' to 'Text'.
--
-- The output is of the form __@A123456789@__.
--
-- This function satisfies the following law:
--
-- @
-- 'fromText' ('toText' i) '==' 'Right' i
-- @
--
toText :: ID -> Text
toText = U.toText . toUnchecked

-- | Converts an 'ID' to a tuple.
--
-- This function satisfies the following laws:
--
-- @
-- 'fromTuple' ('toTuple' i) '==' i
-- @
-- @
-- 'toTuple' ('fromTuple' t) '==' t
-- @
--
toTuple :: Digit ~ d => ID -> (Letter, Digit1289, d, d, d, d, d, d, d)
toTuple (ID c0 c1 c2 c3 c4 c5 c6 c7 c8) =
  (c0, c1, c2, c3, c4, c5, c6, c7, c8)

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

-- | Computes the checksum digit for an 'ID'.
--
checksumDigit :: ID -> Digit
checksumDigit (ID u0 u1 u2 u3 u4 u5 u6 u7 u8) =
  negate $ U.checksum (UncheckedID u0 u1 u2 u3 u4 u5 u6 u7 u8 0)

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- | Generates a random 'ID'.
--
generate :: MonadRandom m => m ID
generate =
  ID
    <$> Letter.generate
    <*> Digit1289.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate
    <*> Digit.generate

--------------------------------------------------------------------------------
-- Inspection
--------------------------------------------------------------------------------

-- | Decodes the 'Gender' component of an 'ID'.
--
getGender :: ID -> Gender
getGender ID {c1} = fst $ decodeC1 c1

-- | Decodes the 'Location' component of an 'ID'.
--
getLocation :: ID -> Location
getLocation ID {c0} = Location.fromLetter c0

-- | Decodes the 'Nationality' component of an 'ID'.
--
getNationality :: ID -> Nationality
getNationality ID {c1} = snd $ decodeC1 c1

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Updates the 'Gender' component of an 'ID'.
--
setGender :: Gender -> ID -> ID
setGender gender i = i {c1 = encodeC1 (gender, getNationality i)}

-- | Updates the 'Location' component of an 'ID'.
--
setLocation :: Location -> ID -> ID
setLocation location i = i {c0 = Location.toLetter location}

-- | Updates the 'Nationality' component of an 'ID'.
--
setNationality :: Nationality -> ID -> ID
setNationality nationality i = i {c1 = encodeC1 (getGender i, nationality)}

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

decodeC1 :: Digit1289 -> (Gender, Nationality)
decodeC1 = \case
  D1289_1 -> (  Male,    National)
  D1289_2 -> (Female,    National)
  D1289_8 -> (  Male, NonNational)
  D1289_9 -> (Female, NonNational)

encodeC1 :: (Gender, Nationality) -> Digit1289
encodeC1 = \case
  (  Male,    National) -> D1289_1
  (Female,    National) -> D1289_2
  (  Male, NonNational) -> D1289_8
  (Female, NonNational) -> D1289_9

fromUnchecked :: UncheckedID -> Maybe ID
fromUnchecked u@(UncheckedID u0 u1 u2 u3 u4 u5 u6 u7 u8 _) =
    case U.checksumValidity u of
      U.ChecksumValid   -> Just i
      U.ChecksumInvalid -> Nothing
  where
    i = ID u0 u1 u2 u3 u4 u5 u6 u7 u8

toUnchecked :: ID -> UncheckedID
toUnchecked i@(ID u0 u1 u2 u3 u4 u5 u6 u7 u8) =
  UncheckedID u0 u1 u2 u3 u4 u5 u6 u7 u8 (checksumDigit i)
