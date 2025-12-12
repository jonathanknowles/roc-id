{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module ROC.ID.Digit.Typed where

import GHC.TypeLits
import Data.Proxy
import ROC.ID.Letter (Letter(..))
import Data.Type.Bool (type (&&))
import GHC.TypeError
import Data.Type.Equality (type (==))
import qualified GHC.TypeNats

----------------------------
-- Digits
----------------------------

data AnyDigit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Ord, Show)

data AnyDigit1289 = D1289_1 | D1289_2 | D1289_8 | D1289_9
  deriving (Bounded, Enum, Eq, Ord, Show)

-- Constraints on type-level digits
type Digit n = (KnownNat n, Assert (0 <=? n && n <=? 9) (TypeError (Text "Digit out of range [0..9]")))
type Digit1289 n = (KnownNat n, Assert (Elem n '[1,2,8,9]) (TypeError (Text "Digit must be in {1,2,8,9}")))

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[]       = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

----------------------------
-- Phantom ROC ID type
----------------------------

data Id (l :: Letter)
           (d0 :: Nat) (d1 :: Nat) (d2 :: Nat) (d3 :: Nat)
           (d4 :: Nat) (d5 :: Nat) (d6 :: Nat) (d7 :: Nat) (d8 :: Nat)
  = Id

----------------------------
-- ROC ID checksum at type level
----------------------------

-- Map first letter to number according to ROC ID rules
type family LetterValue (l :: Letter) :: Nat where
  LetterValue 'A = 10; LetterValue 'B = 11; LetterValue 'C = 12; LetterValue 'D = 13
  LetterValue 'E = 14; LetterValue 'F = 15; LetterValue 'G = 16; LetterValue 'H = 17
  LetterValue 'I = 34; LetterValue 'J = 18; LetterValue 'K = 19; LetterValue 'L = 20
  LetterValue 'M = 21; LetterValue 'N = 22; LetterValue 'O = 35; LetterValue 'P = 23
  LetterValue 'Q = 24; LetterValue 'R = 25; LetterValue 'S = 26; LetterValue 'T = 27
  LetterValue 'U = 28; LetterValue 'V = 29; LetterValue 'W = 32; LetterValue 'X = 30
  LetterValue 'Y = 31; LetterValue 'Z = 33

-- Split a two-digit number into tens and ones

type family Tens (n :: Nat) :: Nat where
  Tens n = Div n 10

type family Ones (n :: Nat) :: Nat where
  Ones n = Mod n 10

-- ROC ID checksum calculation
-- c0..c9 are the weights for each position


type IdChecksum l d0 d1 d2 d3 d4 d5 d6 d7 d8 =
      (Tens (LetterValue l) GHC.TypeNats.* 1)
    GHC.TypeNats.+ (Ones (LetterValue l) GHC.TypeNats.* 9)
    GHC.TypeNats.+ (d0 GHC.TypeNats.* 8)
    GHC.TypeNats.+ (d1 GHC.TypeNats.* 7)
    GHC.TypeNats.+ (d2 GHC.TypeNats.* 6)
    GHC.TypeNats.+ (d3 GHC.TypeNats.* 5)
    GHC.TypeNats.+ (d4 GHC.TypeNats.* 4)
    GHC.TypeNats.+ (d5 GHC.TypeNats.* 3)
    GHC.TypeNats.+ (d6 GHC.TypeNats.* 2)
    GHC.TypeNats.+ (d7 GHC.TypeNats.* 1)
    GHC.TypeNats.+ (d8 GHC.TypeNats.* 1)

-- Constraint that ensures checksum is divisible by 10
type IdValid l d0 d1 d2 d3 d4 d5 d6 d7 d8 =
  Assert (Mod (IdChecksum l d0 d1 d2 d3 d4 d5 d6 d7 d8) 10 == 0)
         (TypeError (Text "Invalid checksum"))

----------------------------
-- Existential wrapper
----------------------------

data AnyId where
  AnyId ::
    ( Digit1289 d0
    , Digit d1
    , Digit d2
    , Digit d3
    , Digit d4
    , Digit d5
    , Digit d6
    , Digit d7
    , Digit d8
    , IdValid l d0 d1 d2 d3 d4 d5 d6 d7 d8
    )
    => Id l d0 d1 d2 d3 d4 d5 d6 d7 d8
    -> AnyId

data AnyIdRep = AnyIdRep
  { idLetterR :: Letter
  , idD1289R  :: AnyDigit1289
  , idDigitsR :: (AnyDigit, AnyDigit, AnyDigit, AnyDigit, AnyDigit, AnyDigit, AnyDigit, AnyDigit)
  }
  deriving (Eq, Show)

-- Convert existential AnyId to runtime representation
anyIdRep :: AnyId -> AnyIdRep
anyIdRep (AnyId (Id :: Id l d0 d1 d2 d3 d4 d5 d6 d7 d8)) =
  AnyIdRep
    { idLetterR = anyLetter @l
    , idD1289R  = anyDigit1289 (Proxy @d0)
    , idDigitsR = ( anyDigit (Proxy @d1)
                  , anyDigit (Proxy @d2)
                  , anyDigit (Proxy @d3)
                  , anyDigit (Proxy @d4)
                  , anyDigit (Proxy @d5)
                  , anyDigit (Proxy @d6)
                  , anyDigit (Proxy @d7)
                  , anyDigit (Proxy @d8)
                  )
    }

-- Show instance
instance Show AnyId where
  show = show . anyIdRep

-- Eq instance
instance Eq AnyId where
  a == b = anyIdRep a == anyIdRep b

----------------------------
-- Convert to runtime representation
----------------------------

anyLetter :: forall l. Letter
anyLetter = toEnum (fromEnum (Proxy @l))

anyDigit :: forall n. KnownNat n => Proxy n -> AnyDigit
anyDigit _ = toEnum (fromInteger (natVal (Proxy @n)))

anyDigit1289 :: forall n. KnownNat n => Proxy n -> AnyDigit1289
anyDigit1289 _ = case natVal (Proxy @n) of
  1 -> D1289_1
  2 -> D1289_2
  8 -> D1289_8
  9 -> D1289_9
  _ -> error "Impossible: ensured by Digit1289"
{-
-- Convert Id to runtime AnyId
toAnyId :: forall l d0 d1 d2 d3 d4 d5 d6 d7 d8.
           (Digit1289 d0, Digit d1, Digit d2, Digit d3, Digit d4,
            Digit d5, Digit d6, Digit d7, Digit d8)
         => Id l d0 d1 d2 d3 d4 d5 d6 d7 d8 -> AnyId
toAnyId _ = AnyId (Id :: Id l d0 d1 d2 d3 d4 d5 d6 d7 d8)
-}
----------------------------
-- Example usage
----------------------------

exampleId :: AnyId
exampleId = AnyId (Id :: Id A 1 2 3 4 5 6 7 8 9)
-- The above will only compile if the checksum is correct
