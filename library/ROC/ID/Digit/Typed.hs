{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ROC.ID.Digit.Typed where

import GHC.TypeLits (TypeError, KnownNat, natVal, type (<=?))
import Data.Proxy (Proxy (Proxy))
import Data.Type.Bool (type (&&))
import GHC.TypeError (Assert, ErrorMessage (Text))
import ROC.ID.Letter (Letter (..))
import GHC.TypeNats (Nat)

data AnyDigit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data AnyDigit1289 = D1289_1 | D1289_2 | D1289_8 | D1289_9
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

type Digit n = (KnownNat n, Assert (DigitRange n) DigitError)
type DigitRange n = (0 <=? n && n <=? 9)
type DigitError = TypeError (Text "Must be a digit in the range [0 .. 9]")

type Digit1289 n = (KnownNat n, Assert (Digit1289Range n) Digit1289Error)
type Digit1289Range n = Elem n '[1, 2, 8, 9]
type Digit1289Error = TypeError (Text "Must be a digit in the set {1, 2, 8, 9}")

data AnyId = AnyId
  { idLetter :: Letter
  , idD1289  :: AnyDigit1289
  , idDigits :: ( AnyDigit, AnyDigit, AnyDigit, AnyDigit
                , AnyDigit, AnyDigit, AnyDigit, AnyDigit )
  } deriving Show

data Id
  (l :: Letter)
  (d0 :: Nat)
  (d1 :: Nat)
  (d2 :: Nat)
  (d3 :: Nat)
  (d4 :: Nat)
  (d5 :: Nat)
  (d6 :: Nat)
  (d7 :: Nat)
  (d8 :: Nat)
  = Id

anyLetter :: forall l. Proxy l -> Letter
anyLetter _ = toEnum $ fromEnum (Proxy @l)  -- simple if Letter is Enum

mkAnyId
  :: forall l d0 d1 d2 d3 d4 d5 d6 d7 d8.
     ( Digit1289 d0
     , Digit d1, Digit d2, Digit d3, Digit d4
     , Digit d5, Digit d6, Digit d7, Digit d8
     )
  => Proxy (Id l d0 d1 d2 d3 d4 d5 d6 d7 d8)
  -> AnyId
mkAnyId _ = AnyId
  { idLetter = anyLetter (Proxy @l)
  , idD1289  = anyDigit1289 (Proxy @d0)
  , idDigits = ( anyDigit (Proxy @d1), anyDigit (Proxy @d2)
               , anyDigit (Proxy @d3), anyDigit (Proxy @d4)
               , anyDigit (Proxy @d5), anyDigit (Proxy @d6)
               , anyDigit (Proxy @d7), anyDigit (Proxy @d8)
               )
  }


exampleId :: AnyId
exampleId = mkAnyId (Proxy @(Id A 1 0 2 3 4 5 6 7 8))


anyDigit :: forall n. Digit n => Proxy n -> AnyDigit
anyDigit _ = toEnum $ fromInteger (natVal (Proxy @n))

anyDigit1289 :: forall n. Digit1289 n => Proxy n -> AnyDigit1289
anyDigit1289 _ = case natVal (Proxy @n) of
  1 -> D1289_1
  2 -> D1289_2
  8 -> D1289_8
  9 -> D1289_9
  _ -> error "Impossible: IsDigit1289 ensures n ∈ {1, 2, 8, 9}"

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[]       = False
  Elem x (x ': xs) = True
  Elem x (_ ': xs) = Elem x xs
