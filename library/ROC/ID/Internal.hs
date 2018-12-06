{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module ROC.ID.Internal where

import Control.Monad.Random.Class (MonadRandom (..))
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Tuple.Only (Only (..))
import Data.Vector.Sized (Vector)
import GHC.Generics (Generic)

import qualified Data.Text         as T
import qualified Data.Vector.Sized as V

-- Types:

-- | Represents a valid 10-digit ROC national identification number
-- (中華民國身份證號碼) of the form __@A123456789@__.
--
-- By construction, invalid values are __not representable__ by this type.
--
-- An identification number encodes a person's 'Gender', the 'Location' in
-- which they first registered for an identification card, and a unique 'Serial'
-- number.
--
data Identity = Identity
  { idGender   :: Gender
  -- ^ The gender of the person to whom this ID number belongs.
  , idLocation :: Location
  -- ^ The location in which the person first registered for an ID card.
  , idSerial   :: Serial
  -- ^ The serial number portion of this ID number.
  } deriving (Eq, Generic, Ord)

-- | A person's gender, as encoded within an 'Identity'.
--
data Gender = Male | Female
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- | A location, as encoded within an 'Identity'.
--
-- To generate the name of a location, see 'printLocation'.
--
data Location
  = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | A 7-digit serial number that is unique for a given 'Gender' and 'Location'.
--
newtype Serial = Serial (Vector 7 Digit)
  deriving (Eq, Generic, Ord, Show)

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Generic, Ord)

-- Encoding:

class Encodable t n | t -> n where
  encode :: t -> Vector n Digit

instance Encodable Gender 1 where
  encode = V.fromTuple . Only . \case
    Male   -> D1
    Female -> D2

instance Encodable Location 2 where
  encode = V.fromTuple . \case
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

instance Encodable Serial 7 where
  encode (Serial c) = c

-- Randomization:

-- | Generate a random 'Identity'.
--
randomIdentity :: MonadRandom m => m Identity
randomIdentity = Identity <$> randomGender
                          <*> randomLocation
                          <*> randomSerial

-- | Generate a random 'Gender'.
--
randomGender :: MonadRandom m => m Gender
randomGender = randomBoundedEnum

-- | Generate a random 'Location'.
--
randomLocation :: MonadRandom m => m Location
randomLocation = randomBoundedEnum

-- | Generate a random 'Serial' number.
--
randomSerial :: MonadRandom m => m Serial
randomSerial = Serial <$> V.replicateM randomBoundedEnum

-- Validation:

calculateChecksum :: Identity -> Digit
calculateChecksum Identity {..} = toEnum $ negate total `mod` 10
  where
    total = 1 * p 0 + 9 * p 1 + 8 * g 0 + 7 * s 0 + 6 * s 1
          + 5 * s 2 + 4 * s 3 + 3 * s 4 + 2 * s 5 + 1 * s 6
    g = index idGender
    p = index idLocation
    s = index idSerial
    index x = fromEnum . V.index e
      where
        e = encode x

-- Parsing:

-- | Attempt to parse an 'Identity' using the specified 'Text' as input.
--
parseIdentity :: Text -> Either ParseError Identity
parseIdentity t = do
    v <-              guard InvalidLength   (parseRaw                     t)
    i <- Identity <$> guard InvalidGender   (parseGender   $ readGender   v)
                  <*> guard InvalidLocation (parseLocation $ readLocation v)
                  <*> guard InvalidSerial   (parseSerial   $ readSerial   v)
    c <-              guard InvalidChecksum (parseDigit    $ readChecksum v)
    if c == calculateChecksum i then pure i else Left InvalidChecksum
  where
    readSerial   = V.slice (Proxy :: Proxy 2)
    readLocation = flip V.index 0
    readGender   = flip V.index 1
    readChecksum = flip V.index 9

-- | An error produced when parsing an 'Identity' with the 'parseIdentity'
--   function.
--
data ParseError
  = InvalidLength
    -- ^ The input was either too short or too long.
  | InvalidGender
    -- ^ The gender portion of the input was invalid.
  | InvalidLocation
    -- ^ The location portion of the input included non-alphabetic characters.
  | InvalidSerial
    -- ^ The serial number portion of the input included non-numeric characters.
  | InvalidChecksum
    -- ^ The computed checksum did not match the checksum portion of the input.
  deriving (Eq, Show)

parseRaw :: Text -> Maybe (Vector 10 Char)
parseRaw  = V.fromList . T.unpack

parseDigit :: Char -> Maybe Digit
parseDigit c = maybeRead [c] >>= maybeToEnum

parseGender :: Char -> Maybe Gender
parseGender = \case
  '1' -> pure Male
  '2' -> pure Female
  _   -> Nothing

parseLocation :: Char -> Maybe Location
parseLocation c = maybeRead [c]

parseSerial :: Vector 7 Char -> Maybe Serial
parseSerial a = Serial <$> traverse parseDigit a

-- Presentation:

instance Show Identity where
  show i@Identity {..} = ""
    <> show idLocation
    <> foldMap show (encode idGender)
    <> foldMap show (encode idSerial)
    <> show (calculateChecksum i)

instance Show Digit where show = show . fromEnum

-- | A language into which values can be localized when pretty printing.
--
data Language = English | Chinese

-- | Pretty-print the specified 'Gender'.
--
printGender :: Language -> Gender -> Text
printGender = \case
  English -> printGenderEnglish
  Chinese -> printGenderChinese

printGenderEnglish :: Gender -> Text
printGenderEnglish = \case
  Male   -> "Male"
  Female -> "Female"

printGenderChinese :: Gender -> Text
printGenderChinese = \case
  Male   -> "男性"
  Female -> "女性"

-- | Pretty-print the specified 'Location'.
printLocation :: Language -> Location -> Text
printLocation = \case
  English -> printLocationEnglish
  Chinese -> printLocationChinese

printLocationChinese :: Location -> Text
printLocationChinese = \case
  A -> "臺北市"
  B -> "臺中市"
  C -> "基隆市"
  D -> "臺南市"
  E -> "高雄市"
  F -> "新北市"
  G -> "宜蘭縣"
  H -> "桃園市"
  I -> "嘉義市"
  J -> "新竹縣"
  K -> "苗栗縣"
  L -> "臺中縣"
  M -> "南投縣"
  N -> "彰化縣"
  O -> "新竹市"
  P -> "雲林縣"
  Q -> "嘉義縣"
  R -> "臺南縣"
  S -> "高雄縣"
  T -> "屏東縣"
  U -> "花蓮縣"
  V -> "臺東縣"
  W -> "金門縣"
  X -> "澎湖縣"
  Y -> "陽明山管理局"
  Z -> "連江縣"

printLocationEnglish :: Location -> Text
printLocationEnglish = \case
  A -> "Taipei City"
  B -> "Taichung City"
  C -> "Keelung City"
  D -> "Tainan City"
  E -> "Kaohsiung City"
  F -> "New Taipei City"
  G -> "Yilan County"
  H -> "Taoyuan City"
  I -> "Chiayi City"
  J -> "Hsinchu County"
  K -> "Miaoli County"
  L -> "Taichung County"
  M -> "Nantou County"
  N -> "Changhua County"
  O -> "Hsinchu City"
  P -> "Yunlin County"
  Q -> "Chiayi County"
  R -> "Pingtung County"
  S -> "Kaohsiung County"
  T -> "Pingtung County"
  U -> "Hualien County"
  V -> "Taitung County"
  W -> "Kinmen County"
  X -> "Penghu County"
  Y -> "Yangmingshan Management Bureau"
  Z -> "Lienchiang County"

-- Utilities:

guard :: x -> Maybe y -> Either x y
guard x = maybe (Left x) Right

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

maybeToEnum :: forall a . Bounded a => Enum a => Int -> Maybe a
maybeToEnum i
  | i < fromEnum (minBound :: a) = Nothing
  | i > fromEnum (maxBound :: a) = Nothing
  | otherwise                    = pure $ toEnum i

randomBoundedEnum :: forall a m . MonadRandom m => Bounded a => Enum a => m a
randomBoundedEnum =
  toEnum <$> getRandomR (fromEnum (minBound :: a), fromEnum (maxBound :: a))

