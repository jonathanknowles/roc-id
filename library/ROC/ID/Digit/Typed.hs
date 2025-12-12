{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}


module ROC.ID.Digit.Typed where

import GHC.TypeLits
import Data.Proxy
import ROC.ID.Letter (Letter(..))
import Data.Type.Bool (type (&&))
import GHC.TypeError
import Data.Type.Equality (type (==))

import qualified GHC.TypeNats as T

data AnyDigit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Ord, Show)

data AnyDigit1289 = D1289_1 | D1289_2 | D1289_8 | D1289_9
  deriving (Bounded, Enum, Eq, Ord, Show)

type Digit n =
  ( KnownNat n
  , Assert
    (0 <=? n && n <=? 9)
    (TypeError (Text "Digit must be in the range [0 .. 9]"))
  )

type Digit1289 n =
  ( KnownNat n
  , Assert
    (Elem n '[1, 2, 8, 9])
    (TypeError (Text "Digit must be in {1, 2, 8, 9}"))
  )

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

type family LetterValues (l :: Letter) :: (Nat, Nat) where
  LetterValues 'A = '(1, 0); LetterValues 'B = '(1, 1)
  LetterValues 'C = '(1, 2); LetterValues 'D = '(1, 3)
  LetterValues 'E = '(1, 4); LetterValues 'F = '(1, 5)
  LetterValues 'G = '(1, 6); LetterValues 'H = '(1, 7)
  LetterValues 'I = '(3, 4); LetterValues 'J = '(1, 8)
  LetterValues 'K = '(1, 9); LetterValues 'L = '(2, 0)
  LetterValues 'M = '(2, 1); LetterValues 'N = '(2, 2)
  LetterValues 'O = '(3, 5); LetterValues 'P = '(2, 3)
  LetterValues 'Q = '(2, 4); LetterValues 'R = '(2, 5)
  LetterValues 'S = '(2, 6); LetterValues 'T = '(2, 7)
  LetterValues 'U = '(2, 8); LetterValues 'V = '(2, 9)
  LetterValues 'W = '(3, 2); LetterValues 'X = '(3, 0)
  LetterValues 'Y = '(3, 1); LetterValues 'Z = '(3, 3)

type family LetterValue0 letter :: Nat where
  LetterValue0 x = Fst (LetterValues x)

type family LetterValue1 letter :: Nat where
  LetterValue1 x = Snd (LetterValues x)

type family Fst (t :: (T.Nat, T.Nat)) :: T.Nat where
  Fst '(x, _) = x

type family Snd (t :: (T.Nat, T.Nat)) :: T.Nat where
  Snd '(_, y) = y

type Checksum letter d0 d1 d2 d3 d4 d5 d6 d7 d8
  = LastDigit
    (     (LetterValue0 letter T.* 1)
      T.+ (LetterValue1 letter T.* 9)
      T.+ (d0                  T.* 8)
      T.+ (d1                  T.* 7)
      T.+ (d2                  T.* 6)
      T.+ (d3                  T.* 5)
      T.+ (d4                  T.* 4)
      T.+ (d5                  T.* 3)
      T.+ (d6                  T.* 2)
      T.+ (d7                  T.* 1)
      T.+ (d8                  T.* 1)
    )

type family LastDigit (n :: Nat) where
  LastDigit n = Mod n 10

type ChecksumValid l d0 d1 d2 d3 d4 d5 d6 d7 d8 =
  Assert (Checksum l d0 d1 d2 d3 d4 d5 d6 d7 d8 == 0)
         (TypeError (Text "Invalid checksum"))

----------------------------
-- Existential wrapper
----------------------------

type Valid l d0 d1 d2 d3 d4 d5 d6 d7 d8 =
    ( Digit1289 d0
    , Digit d1
    , Digit d2
    , Digit d3
    , Digit d4
    , Digit d5
    , Digit d6
    , Digit d7
    , Digit d8
    , ChecksumValid l d0 d1 d2 d3 d4 d5 d6 d7 d8
    )

data AnyId where
  AnyId ::
    Valid l d0 d1 d2 d3 d4 d5 d6 d7 d8
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

----------------------------
-- Example usage
----------------------------

exampleId :: AnyId
exampleId = AnyId (Id :: Id A 1 2 3 4 5 6 7 8 9)
-- The above will only compile if the checksum is correct
