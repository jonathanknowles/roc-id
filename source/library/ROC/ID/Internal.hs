{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module ROC.ID.Internal where

import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Tuple.Only (Only (..))
import Data.Vector.Sized (Vector)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat (..))

import qualified Data.Text         as T
import qualified Data.Vector.Sized as V

-- Types:

data Identity = Identity
  { idGender   :: Gender
  , idLocation :: Location
  , idSerial   :: Serial
  } deriving (Eq, Generic, Ord)

data Gender = Male | Female
  deriving (Eq, Generic, Ord, Show)

data Location
  = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Generic, Ord, Read, Show)

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

-- Validation:

calculateChecksum :: Identity -> Digit
calculateChecksum Identity {..} = toEnum $ negate sum `mod` 10
  where
    sum = 1 * p 0 + 9 * p 1 + 8 * g 0 + 7 * s 0 + 6 * s 1
        + 5 * s 2 + 4 * s 3 + 3 * s 4 + 2 * s 5 + 1 * s 6
    g = index idGender
    p = index idLocation
    s = index idSerial
    index x = fromEnum . V.index e
      where
        e = encode x

-- Parsing:

data ParseError
  = InvalidLength
  | InvalidGender
  | InvalidLocation
  | InvalidSerial
  | InvalidChecksum
  deriving (Eq, Show)

parseIdentity :: Text -> Either ParseError Identity
parseIdentity t = do
    v <-              guard InvalidLength   (parseRaw                     t)
    c <-              guard InvalidChecksum (parseDigit    $ readChecksum v)
    i <- Identity <$> guard InvalidGender   (parseGender   $ readGender   v)
                  <*> guard InvalidLocation (parseLocation $ readLocation v)
                  <*> guard InvalidSerial   (parseSerial   $ readSerial   v)
    if c == calculateChecksum i then pure i else Left InvalidChecksum
  where
    readSerial   = V.slice (Proxy :: Proxy 2)
    readLocation = flip V.index 0
    readGender   = flip V.index 1
    readChecksum = flip V.index 9

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

data Language = English | Chinese

genderText :: Language -> Gender -> Text
genderText = \case
  English -> genderTextEnglish
  Chinese -> genderTextChinese

genderTextEnglish :: Gender -> Text
genderTextEnglish = \case
  Male   -> "Male"
  Female -> "Female"

genderTextChinese :: Gender -> Text
genderTextChinese = \case
  Male   -> "男性"
  Female -> "女性"

locationText :: Language -> Location -> Text
locationText = \case
  English -> locationTextEnglish
  Chinese -> locationTextChinese

locationTextChinese :: Location -> Text
locationTextChinese = \case
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

locationTextEnglish :: Location -> Text
locationTextEnglish = \case
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

