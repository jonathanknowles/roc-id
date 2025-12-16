{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module ROC.ID.Unchecked
  ( UncheckedID (..)
  , UncheckedIDTuple
  , FromTextError (..)
  , CharIndex (..)
  , CharSet (..)
  , fromText
  , toText
  , checksum
  , checksumValidity
  , ChecksumValidity (..)
  , ValidID
  )
  where

import Control.Monad
  ( when )
import Data.Kind
  ( Constraint )
import Data.List.NonEmpty
  ( NonEmpty ((:|)) )
import Data.Set.NonEmpty
  ( NESet )
import Data.Text
  ( Text )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeError
  ( Assert, TypeError )
import GHC.TypeLits
  ( KnownSymbol, Symbol )
import GHC.TypeNats
  ( Mod, Nat )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Digit1289
  ( Digit1289 (..) )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Utilities
  ( Fst, Snd, SymbolToCharList, guard )

import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import qualified GHC.TypeError as TypeError
import qualified GHC.TypeNats as N
import qualified ROC.ID.Digit as Digit
import qualified ROC.ID.Digit1289 as Digit1289
import qualified ROC.ID.Letter as Letter

data UncheckedID = UncheckedID
  { c0 :: !Letter
  , c1 :: !Digit1289
  , c2 :: !Digit
  , c3 :: !Digit
  , c4 :: !Digit
  , c5 :: !Digit
  , c6 :: !Digit
  , c7 :: !Digit
  , c8 :: !Digit
  , c9 :: !Digit
  }
  deriving stock (Eq, Ord, Show)

type UncheckedIDTuple =
  ( Letter
  , Digit1289
  , Digit
  , Digit
  , Digit
  , Digit
  , Digit
  , Digit
  , Digit
  , Digit
  )

-- | Specifies the zero-based position of a character within a string.
--
newtype CharIndex = CharIndex Int
  deriving stock (Read, Show)
  deriving newtype (Enum, Eq, Num, Ord)

-- | Specifies a set of characters.
--
data CharSet
  = CharSet (NESet Char)
  -- ^ An explicit set of characters.
  | CharRange Char Char
  -- ^ An inclusive range of characters.
  deriving stock (Eq, Ord, Read, Show)

data FromTextError
  = TextTooShort
  | TextTooLong
  | InvalidChar CharIndex CharSet
  deriving stock (Eq, Ord, Read, Show)

type Parser a = Text -> Either FromTextError (Text, a)

fromText :: Text -> Either FromTextError UncheckedID
fromText text0 = do
    when (T.length text0 > 10) $ Left TextTooLong
    (text1,  c0                             ) <- parseLetter    text0
    (text2,  c1                             ) <- parseDigit1289 text1
    (_____, (c2, c3, c4, c5, c6, c7, c8, c9)) <- parseDigits    text2
    pure UncheckedID {c0, c1, c2, c3, c4, c5, c6, c7, c8, c9}
  where
    parseLetter :: Parser Letter
    parseLetter text = do
      (char, remainder) <- guard TextTooShort $ T.uncons text
      letter <- guard (InvalidChar 0 (CharRange 'A' 'Z')) (Letter.fromChar char)
      pure (remainder, letter)

    parseDigit1289 :: Parser Digit1289
    parseDigit1289 text = do
      (char, remainder) <- guard TextTooShort $ T.uncons text
      digit1289 <- guard
        (InvalidChar 1 (CharSet $ NESet.fromList $ '1' :| ['2', '8', '9']))
        (Digit1289.fromChar char)
      pure (remainder, digit1289)

    parseDigits :: d ~ Digit => Parser (d, d, d, d, d, d, d, d)
    parseDigits text = do
        let (chars, remainder) = T.splitAt 8 text
        digitList <- traverse parseIndexedDigit (zip [2 ..] $ T.unpack chars)
        digitTuple <- guard TextTooShort (listToTuple8 digitList)
        pure (remainder, digitTuple)
      where
        parseIndexedDigit (i, c) =
          guard (InvalidChar i (CharRange '0' '9')) (Digit.fromChar c)

toText :: UncheckedID -> Text
toText UncheckedID {c0, c1, c2, c3, c4, c5, c6, c7, c8, c9} =
  T.pack
    ( Letter.toChar c0
    : Digit1289.toChar c1
    : fmap Digit.toChar [c2, c3, c4, c5, c6, c7, c8, c9]
    )

checksum :: UncheckedID -> Digit
checksum (UncheckedID u0 (Digit1289.toDigit -> u1) u2 u3 u4 u5 u6 u7 u8 u9) =
  sum $ zipWith (*)
    [ 1,  9,  8,  7,  6,  5,  4,  3,  2,  1,  1]
    [a0, a1, u1, u2, u3, u4, u5, u6, u7, u8, u9]
  where
    (a0, a1) = checksumLetterToDigitPair u0

checksumLetterToDigitPair :: Letter -> (Digit, Digit)
checksumLetterToDigitPair = \case
  A -> (1, 0); N -> (2, 2)
  B -> (1, 1); O -> (3, 5)
  C -> (1, 2); P -> (2, 3)
  D -> (1, 3); Q -> (2, 4)
  E -> (1, 4); R -> (2, 5)
  F -> (1, 5); S -> (2, 6)
  G -> (1, 6); T -> (2, 7)
  H -> (1, 7); U -> (2, 8)
  I -> (3, 4); V -> (2, 9)
  J -> (1, 8); W -> (3, 2)
  K -> (1, 9); X -> (3, 0)
  L -> (2, 0); Y -> (3, 1)
  M -> (2, 1); Z -> (3, 3)

data ChecksumValidity
  = ChecksumValid
  | ChecksumInvalid
  deriving (Eq, Show)

checksumValidity :: UncheckedID -> ChecksumValidity
checksumValidity u =
  case checksum u of
    0 -> ChecksumValid
    _ -> ChecksumInvalid

listToTuple8 :: [a] -> Maybe (a, a, a, a, a, a, a, a)
listToTuple8 = \case
  (a0 : a1 : a2 : a3 : a4 : a5 : a6 : a7 : _) ->
    Just (a0, a1, a2, a3, a4, a5, a6, a7)
  _ ->
    Nothing

type family ChecksumValid id :: Constraint where
  ChecksumValid id =
    Assert
      (ChecksumDigit id == D0)
      (TypeError (TypeError.Text "ID has invalid checksum."))

type family ChecksumDigit id :: Digit where
  ChecksumDigit id = Digit.FromNat (Mod (Checksum id) 10)

type family Checksum (id :: UncheckedIDTuple) :: Nat where
  Checksum '(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9) =
    (     (ChecksumLetterToNat0 c0 N.* 1)
      N.+ (ChecksumLetterToNat1 c0 N.* 9)
      N.+ (Digit1289.ToNat      c1 N.* 8)
      N.+ (Digit.ToNat          c2 N.* 7)
      N.+ (Digit.ToNat          c3 N.* 6)
      N.+ (Digit.ToNat          c4 N.* 5)
      N.+ (Digit.ToNat          c5 N.* 4)
      N.+ (Digit.ToNat          c6 N.* 3)
      N.+ (Digit.ToNat          c7 N.* 2)
      N.+ (Digit.ToNat          c8 N.* 1)
      N.+ (Digit.ToNat          c9 N.* 1)
    )

type family ChecksumLetterToNat0 (letter :: Letter) :: Nat where
  ChecksumLetterToNat0 x = Fst (ChecksumLetterToNatPair x)

type family ChecksumLetterToNat1 (letter :: Letter) :: Nat where
  ChecksumLetterToNat1 x = Snd (ChecksumLetterToNatPair x)

type family ChecksumLetterToNatPair (l :: Letter) :: (Nat, Nat) where
  ChecksumLetterToNatPair A = '(1, 0); ChecksumLetterToNatPair B = '(1, 1)
  ChecksumLetterToNatPair C = '(1, 2); ChecksumLetterToNatPair D = '(1, 3)
  ChecksumLetterToNatPair E = '(1, 4); ChecksumLetterToNatPair F = '(1, 5)
  ChecksumLetterToNatPair G = '(1, 6); ChecksumLetterToNatPair H = '(1, 7)
  ChecksumLetterToNatPair I = '(3, 4); ChecksumLetterToNatPair J = '(1, 8)
  ChecksumLetterToNatPair K = '(1, 9); ChecksumLetterToNatPair L = '(2, 0)
  ChecksumLetterToNatPair M = '(2, 1); ChecksumLetterToNatPair N = '(2, 2)
  ChecksumLetterToNatPair O = '(3, 5); ChecksumLetterToNatPair P = '(2, 3)
  ChecksumLetterToNatPair Q = '(2, 4); ChecksumLetterToNatPair R = '(2, 5)
  ChecksumLetterToNatPair S = '(2, 6); ChecksumLetterToNatPair T = '(2, 7)
  ChecksumLetterToNatPair U = '(2, 8); ChecksumLetterToNatPair V = '(2, 9)
  ChecksumLetterToNatPair W = '(3, 2); ChecksumLetterToNatPair X = '(3, 0)
  ChecksumLetterToNatPair Y = '(3, 1); ChecksumLetterToNatPair Z = '(3, 3)

type family SymbolToId (s :: Symbol) :: UncheckedIDTuple where
  SymbolToId s =
    IdFromCharList (SymbolToCharList s)

type family IdFromCharList (xs :: [Char]) :: UncheckedIDTuple where
  IdFromCharList '[c0, c1, c2, c3, c4, c5, c6, c7, c8, c9] =
    '( Letter.FromChar    c0
     , Digit1289.FromChar c1
     , Digit.FromChar     c2
     , Digit.FromChar     c3
     , Digit.FromChar     c4
     , Digit.FromChar     c5
     , Digit.FromChar     c6
     , Digit.FromChar     c7
     , Digit.FromChar     c8
     , Digit.FromChar     c9
     )
  IdFromCharList _ =
    TypeError (TypeError.Text "An ID must have exactly 10 characters.")

type ValidID s =
  ( KnownSymbol s
  , ChecksumValid (SymbolToId s)
  ) :: Constraint
