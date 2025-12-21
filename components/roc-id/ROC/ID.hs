{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

  -- * Conversion
  , toText

  -- * Validity
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
import ROC.ID.CharIndex
  ( CharIndex (..) )
import ROC.ID.CharSet
  ( CharSet (..) )
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
  ( UncheckedID (UncheckedID)
  , ValidID
  )
import ROC.ID.Utilities
  ( guard )
import Text.Read
  ( Lexeme (Ident, Symbol, Punc), Read (readPrec), lexP, parens )

import qualified Data.Text as T
import qualified ROC.ID.Digit as Digit
import qualified ROC.ID.Digit1289 as Digit1289
import qualified ROC.ID.Letter as Letter
import qualified ROC.ID.Location as Location
import qualified ROC.ID.Unchecked as U

-- |
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> import ROC.ID
-- >>> import qualified ROC.ID as ID

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Represents a __valid__ 10-digit ROC (Taiwan) Uniform Identification Number
-- (中華民國統一證號) of the form __@A123456789@__.
--
-- By construction, invalid identification numbers are __not representable__ by
-- this data type.
--
-- To guarantee correctness, an 'ID' value does __not__ store the terminal
-- checksum digit of the identification number it represents.
--
-- To compute the checksum digit, use 'checksumDigit'.
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
  deriving (Eq, Ord)

instance Read ID where
  readPrec = parens $ do
    Ident  "ID"         <- lexP
    Symbol "."          <- lexP
    Ident  "fromSymbol" <- lexP
    Punc   "@"          <- lexP
    unsafeFromText <$> readPrec

instance Show ID where
  showsPrec _ s =
    showString "ID.fromSymbol @" . shows (toText s)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Constructs an 'ID' from a type-level textual symbol.
--
-- The symbol must be exactly 10 characters in length and of the form
-- __@A123456789@__:
--
-- >>> ID.fromSymbol @"A123456789"
-- ID.fromSymbol @"A123456789"
--
-- More precisely:
--
--  - the symbol must match the regular expression __@^[A-Z][1289][0-9]{8}$@__
--  - the resultant ID must have a valid checksum.
--
-- Failure to satisfy these constraints will result in one of the following
-- __type errors__:
--
-- === Invalid lengths
--
-- >>> ID.fromSymbol @"A12345678"
-- ...
-- ... An ID must have exactly 10 characters.
-- ...
--
-- === Invalid checksums
--
-- >>> ID.fromSymbol @"A123456780"
-- ...
-- ... ID has invalid checksum.
-- ...
--
-- === Invalid characters
--
-- On detection of an invalid character, the resulting type error reports
-- both the position of the character and the set of characters permitted
-- at that position.
--
-- >>> ID.fromSymbol @"_123456789"
-- ...
--     • "_123456789"
--        ^
--       Character at this position must be an uppercase letter.
-- ...
--
-- >>> ID.fromSymbol @"A_23456789"
-- ...
--     • "A_23456789"
--         ^
--       Character at this position must be a digit from the set {1, 2, 8, 9}.
-- ...
--
-- >>> ID.fromSymbol @"A1_3456789"
-- ...
--     • "A1_3456789"
--          ^
--       Character at this position must be a digit in the range [0 .. 9].
-- ...
--
fromSymbol :: forall (s :: Symbol). ValidID s => ID
fromSymbol = unsafeFromText $ T.pack $ symbolVal $ Proxy @s

-- | Attempts to construct an 'ID' from 'Text'.
--
-- The input must be exactly 10 characters in length and of the form
-- __@A123456789@__.
--
-- More precisely, the input must match the regular expression
-- __@^[A-Z][1289][0-9]{8}$@__, and the resultant ID must have a valid
-- checksum.
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

--------------------------------------------------------------------------------
-- Validity
--------------------------------------------------------------------------------

-- | Computes the terminal checksum digit of an 'ID'.
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
