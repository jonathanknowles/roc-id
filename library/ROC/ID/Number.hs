{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Text
  ( Text )
import ROC.ID.Digit
  ( Digit (..) )
import ROC.ID.Digit1289
  ( Digit1289 )
import ROC.ID.Letter
  ( Letter (..) )
import ROC.ID.Number.Unchecked
  ( CharIndex (..), CharSet (..), UncheckedIdentityNumber (..) )
import ROC.ID.Utilities
  ( guard )

import qualified ROC.ID.Digit1289 as Digit1289
import qualified ROC.ID.Number.Unchecked as U

data IdentityNumber = IdentityNumber
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

-- | Indicates an error that occurred while parsing an identification number.
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

fromText :: Text -> Either FromTextError IdentityNumber
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

toText :: IdentityNumber -> Text
toText = U.toText . toUnchecked

fromUnchecked :: UncheckedIdentityNumber -> Maybe IdentityNumber
fromUnchecked (UncheckedIdentityNumber u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
    | checksum i == u9 = Just i
    | otherwise = Nothing
  where
    i = IdentityNumber u0 u1 u2 u3 u4 u5 u6 u7 u8

toUnchecked :: IdentityNumber -> UncheckedIdentityNumber
toUnchecked i@(IdentityNumber u0 u1 u2 u3 u4 u5 u6 u7 u8) =
  UncheckedIdentityNumber u0 u1 u2 u3 u4 u5 u6 u7 u8 (checksum i)

checksum :: IdentityNumber -> Digit
checksum (IdentityNumber u0 u1 u2 u3 u4 u5 u6 u7 u8) =
    toEnum $ (`mod` 10) $ negate $ sum $ zipWith (*) cs vs
  where
    cs = [1, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    vs = fromEnum <$>
      (a0 : a1 : Digit1289.toDigit u1 : [u2, u3, u4, u5, u6, u7, u8])

    a0, a1 :: Digit
    (a0, a1) = case u0 of
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
