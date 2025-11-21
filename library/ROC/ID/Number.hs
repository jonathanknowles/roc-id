{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module ROC.ID.Number
  ( IdentityNumber (..)
  , FromTextError (..)
  , CharIndex (..)
  , CharSet (..)
  , fromText
  , fromUnchecked
  , toText
  , toUnchecked
  , checksum
  )
  where

import Data.Bifunctor
  ( Bifunctor (first) )
import Data.Vector.Sized
  ( Vector )
import Data.Text
  ( Text )
import ROC.ID.Digit
  ( Digit (..), Digit1289 )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Number.Unchecked
  ( CharIndex (..), CharSet (..), u0, u1, u2 )
import ROC.ID.Utilities
  ( guard )

import qualified Data.Vector.Sized as V
import qualified ROC.ID.Digit as Digit
import qualified ROC.ID.Number.Unchecked as U

data IdentityNumber = IdentityNumber
  { c0 :: !Letter
  , c1 :: !Digit1289
  , c2 :: !(Vector 7 Digit)
  }
  deriving (Eq, Ord, Show)

data FromTextError
  = InvalidLength
  | InvalidChecksum
  | InvalidChar CharIndex CharSet
  deriving (Eq, Ord, Show)

fromText :: Text -> Either FromTextError IdentityNumber
fromText text = do
    unchecked <- first fromUncheckedError $ U.fromText text
    guard InvalidChecksum $ fromUnchecked unchecked
  where
    fromUncheckedError :: U.FromTextError -> FromTextError
    fromUncheckedError = \case
      U.InvalidLength ->
        InvalidLength
      U.InvalidChar i r ->
        InvalidChar i r

toText :: IdentityNumber -> Text
toText = U.toText . toUnchecked

fromUnchecked :: U.IdentityNumber -> Maybe IdentityNumber
fromUnchecked U.IdentityNumber {u0, u1, u2}
    | checksum i == c = Just i
    | otherwise = Nothing
  where
    i = IdentityNumber {c0 = u0, c1 = u1, c2 = V.take @7 u2}
    c = V.last u2

toUnchecked :: IdentityNumber -> U.IdentityNumber
toUnchecked i@IdentityNumber {c0, c1, c2} =
  U.IdentityNumber {u0 = c0, u1 = c1, u2 = c2 V.++ V.singleton (checksum i)}

checksum :: IdentityNumber -> Digit
checksum IdentityNumber {c0, c1, c2} =
    toEnum $ (`mod` 10) $ negate $ sum $ zipWith (*) cs vs
  where
    cs = [1, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    vs = fromEnum <$> V.toList (as V.++ V.cons (Digit.fromDigit1289 c1) c2)

    as :: Vector 2 Digit
    as = V.fromTuple $ case c0 of
      A -> (D1, D0); N -> (D2, D2)
      B -> (D1, D1); O -> (D3, D5)
      C -> (D1, D2); P -> (D2, D3)
      D -> (D1, D3); Q -> (D2, D4)
      E -> (D1, D4); R -> (D2, D5)
      F -> (D1, D5); S -> (D2, D6)
      G -> (D1, D6); T -> (D2, D7)
      H -> (D1, D7); U -> (D2, D8)
      I -> (D3, D4); V -> (D2, D9)
      J -> (D1, D8); W -> (D3, D2)
      K -> (D1, D9); X -> (D3, D0)
      L -> (D2, D0); Y -> (D3, D1)
      M -> (D2, D1); Z -> (D3, D3)
