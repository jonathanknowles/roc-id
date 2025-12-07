{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ROC.ID.Serial.Internal
  ( Serial (..)
  , fromTuple
  , toTuple
  , generate
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import GHC.Generics
  ( Generic )
import ROC.ID.Digit
  ( Digit )
import Text.Read
  ( Lexeme (Ident, Symbol)
  , Read (readPrec)
  , lexP
  , parens
  )

import qualified ROC.ID.Digit as Digit

-- | A 7-digit serial number, as found within an ROC identification number.
--
-- To generate a random 'Serial' number, use the 'generate' function.
--
data Serial = Serial
  { s0 :: !Digit
  , s1 :: !Digit
  , s2 :: !Digit
  , s3 :: !Digit
  , s4 :: !Digit
  , s5 :: !Digit
  , s6 :: !Digit
  }
  deriving (Eq, Generic, Ord)

instance Read Serial where
  readPrec = parens $ do
    Ident "Serial"    <- lexP
    Symbol "."        <- lexP
    Ident "fromTuple" <- lexP
    fromTuple <$> readPrec

instance Show Serial where
  showsPrec _ s =
    showString "Serial.fromTuple " . shows (toTuple s)

-- | Constructs a 'Serial' number from a tuple.
--
fromTuple :: d ~ Digit => (d, d, d, d, d, d, d) -> Serial
fromTuple (s0, s1, s2, s3, s4, s5, s6) = Serial {s0, s1, s2, s3, s4, s5, s6}

-- | Converts a 'Serial' number to a tuple.
--
toTuple :: d ~ Digit => Serial -> (d, d, d, d, d, d, d)
toTuple Serial {s0, s1, s2, s3, s4, s5, s6} = (s0, s1, s2, s3, s4, s5, s6)

-- | Generates a random 'Serial' number.
--
generate :: MonadRandom m => m Serial
generate = Serial
  <$> Digit.generate
  <*> Digit.generate
  <*> Digit.generate
  <*> Digit.generate
  <*> Digit.generate
  <*> Digit.generate
  <*> Digit.generate
