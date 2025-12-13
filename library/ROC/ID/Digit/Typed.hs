{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}


module ROC.ID.Digit.Typed where

import GHC.TypeLits
--import Data.Proxy
import ROC.ID.Letter (Letter(..))
--import Data.Type.Bool (type (&&))
import GHC.TypeError
import Data.Type.Equality (type (==))
import ROC.ID.Digit
import ROC.ID.Digit1289

import qualified GHC.TypeNats as T
import Data.Kind (Constraint)

type family NatIsDigit (n :: Nat) :: Constraint where
  NatIsDigit 0 = ()
  NatIsDigit 1 = ()
  NatIsDigit 2 = ()
  NatIsDigit 3 = ()
  NatIsDigit 4 = ()
  NatIsDigit 5 = ()
  NatIsDigit 6 = ()
  NatIsDigit 7 = ()
  NatIsDigit 8 = ()
  NatIsDigit 9 = ()
  NatIsDigit _ = DigitTypeError

type family NatToDigit (n :: Nat) :: Digit where
  NatToDigit 0 = D0
  NatToDigit 1 = D1
  NatToDigit 2 = D2
  NatToDigit 3 = D3
  NatToDigit 4 = D4
  NatToDigit 5 = D5
  NatToDigit 6 = D6
  NatToDigit 7 = D7
  NatToDigit 8 = D8
  NatToDigit 9 = D9
  NatToDigit _ = DigitTypeError

type DigitTypeError =
  TypeError (Text "Digit must be in the range [0 .. 9]")

type family DigitToNat (d :: Digit) :: Nat where
  DigitToNat D0 = 0
  DigitToNat D1 = 1
  DigitToNat D2 = 2
  DigitToNat D3 = 3
  DigitToNat D4 = 4
  DigitToNat D5 = 5
  DigitToNat D6 = 6
  DigitToNat D7 = 7
  DigitToNat D8 = 8
  DigitToNat D9 = 9

type family NatToDigit1289 (n :: Nat) :: Digit1289 where
  NatToDigit1289 1 = D1289_1
  NatToDigit1289 2 = D1289_2
  NatToDigit1289 8 = D1289_8
  NatToDigit1289 9 = D1289_9
  NatToDigit1289 _ = TypeError (Text "Digit must be in {1, 2, 8, 9}")

type family Digit1289ToNat (d :: Digit1289) :: Nat where
  Digit1289ToNat D1289_1 = 1
  Digit1289ToNat D1289_2 = 2
  Digit1289ToNat D1289_8 = 8
  Digit1289ToNat D1289_9 = 9



{-
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
-}
data Id
  (c0 :: Letter)
  (c1 :: Digit1289)
  (c2 :: Digit)
  (c3 :: Digit)
  (c4 :: Digit)
  (c5 :: Digit)
  (c6 :: Digit)
  (c7 :: Digit)
  (c8 :: Digit)
  (c9 :: Digit)
  = Id

data MkId
  (c0 :: Letter)
  (c1 :: Digit1289)
  (c2 :: Digit)
  (c3 :: Digit)
  (c4 :: Digit)
  (c5 :: Digit)
  (c6 :: Digit)
  (c7 :: Digit)
  (c8 :: Digit)
  (c9 :: Digit)
  = MkId

type family LetterToNatPair (l :: Letter) :: (Nat, Nat) where
  LetterToNatPair 'A = '(1, 0); LetterToNatPair 'B = '(1, 1)
  LetterToNatPair 'C = '(1, 2); LetterToNatPair 'D = '(1, 3)
  LetterToNatPair 'E = '(1, 4); LetterToNatPair 'F = '(1, 5)
  LetterToNatPair 'G = '(1, 6); LetterToNatPair 'H = '(1, 7)
  LetterToNatPair 'I = '(3, 4); LetterToNatPair 'J = '(1, 8)
  LetterToNatPair 'K = '(1, 9); LetterToNatPair 'L = '(2, 0)
  LetterToNatPair 'M = '(2, 1); LetterToNatPair 'N = '(2, 2)
  LetterToNatPair 'O = '(3, 5); LetterToNatPair 'P = '(2, 3)
  LetterToNatPair 'Q = '(2, 4); LetterToNatPair 'R = '(2, 5)
  LetterToNatPair 'S = '(2, 6); LetterToNatPair 'T = '(2, 7)
  LetterToNatPair 'U = '(2, 8); LetterToNatPair 'V = '(2, 9)
  LetterToNatPair 'W = '(3, 2); LetterToNatPair 'X = '(3, 0)
  LetterToNatPair 'Y = '(3, 1); LetterToNatPair 'Z = '(3, 3)

type family LetterToNat0 letter :: Nat where
  LetterToNat0 x = Fst (LetterToNatPair x)

type family LetterToNat1 letter :: Nat where
  LetterToNat1 x = Snd (LetterToNatPair x)

type family Checksum id :: Nat where
  Checksum (Id c0 c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    (     (LetterToNat0    c0 T.* 1)
      T.+ (LetterToNat1    c0 T.* 9)
      T.+ (Digit1289ToNat  c1 T.* 8)
      T.+ (DigitToNat      c2 T.* 7)
      T.+ (DigitToNat      c3 T.* 6)
      T.+ (DigitToNat      c4 T.* 5)
      T.+ (DigitToNat      c5 T.* 4)
      T.+ (DigitToNat      c6 T.* 3)
      T.+ (DigitToNat      c7 T.* 2)
      T.+ (DigitToNat      c8 T.* 1)
      T.+ (DigitToNat      c9 T.* 1)
    )

type family ChecksumDigit id :: Digit where
  ChecksumDigit id = NatToDigit (Mod (Checksum id) 10)

type family ChecksumValid id :: Constraint where
  ChecksumValid id =
    Assert (ChecksumDigit id == D0) (TypeError (Text "Invalid checksum"))

----------------------------
-- Existential wrapper
----------------------------
{-
type Valid c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 =
    ( ChecksumValid
      (Id
        c0
        (NatToDigit1289 c1)
        (NatToDigit c2)
        (NatToDigit c2)
        (NatToDigit c2)
        (NatToDigit c2)
        (NatToDigit c2)
        (NatToDigit c2)
        (NatToDigit c2)
        (NatToDigit c2)
      )
    )

data Id where
  Id ::
    Valid l d0 d1 d2 d3 d4 d5 d6 d7 d8
    => MkId l d0 d1 d2 d3 d4 d5 d6 d7 d8
    -> AnyId

data AnyIdRep = AnyIdRep
  { idLetterR :: Letter
  , idD1289R  :: Digit1289
  , idDigitsR :: (Digit, Digit, Digit, Digit, Digit, Digit, Digit, Digit)
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

anyDigit :: forall n. KnownNat n => Proxy n -> Digit
anyDigit _ = toEnum (fromInteger (natVal (Proxy @n)))

anyDigit1289 :: forall n. KnownNat n => Proxy n -> Digit1289
anyDigit1289 _ = case natVal (Proxy @n) of
  1 -> D1289_1
  2 -> D1289_2
  8 -> D1289_8
  9 -> D1289_9
  _ -> error "Impossible: ensured by Digit1289"

----------------------------
-- Example usage
----------------------------

exampleId :: Id
exampleId = Id (MkId :: MkId A 1 2 3 4 5 6 7 8 9)
-- The above will only compile if the checksum is correct

-}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[]       = 'False
  Elem x (x ': xs) = 'True
  Elem x (_ ': xs) = Elem x xs

type family Fst (t :: (a, a)) :: a where
  Fst '(x, _) = x

type family Snd (t :: (a, a)) :: a where
  Snd '(_, y) = y

type family Seq (a :: k) (b :: l) :: Constraint where
  Seq _ b = ()
