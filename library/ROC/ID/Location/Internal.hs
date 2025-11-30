{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ROC.ID.Location.Internal
  ( Location
  , fromChar
  , fromLetter
  , toLetter
  , toText
  , generate
  ) where

import Control.Monad.Random.Class
  ( MonadRandom (..) )
import Data.Text
  ( Text )
import GHC.Generics
  ( Generic )
import ROC.ID.Language
  ( Language (..) )
import ROC.ID.Letter
  ( Letter (..) )
import Text.Read
  ( readMaybe )

import qualified ROC.ID.Letter as Letter

-- | A location, encodable within an ROC identification number.
--
-- To generate the name of a 'Location', use the 'toText' function.
--
-- To parse a 'Location', use the 'fromChar' function.
--
-- To generate a random 'Location', use the 'generate' function.
--
newtype Location = Location {toLetter :: Letter}
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Bounded, Enum, Read, Show)

-- | Parse the specified uppercase alphabetic character as a 'Location'.
--
-- Returns 'Nothing' if the specified character is not an uppercase alphabetic
-- character.
--
fromChar :: Char -> Maybe Location
fromChar c = readMaybe [c]

fromLetter :: Letter -> Location
fromLetter = Location

-- | Pretty-print the specified 'Location'.
toText :: Language -> Location -> Text
toText = \case
  English -> toTextEnglish
  Chinese -> toTextChinese

toTextChinese :: Location -> Text
toTextChinese (Location letter) = case letter of
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
  Y -> "陽明山"
  Z -> "連江縣"

toTextEnglish :: Location -> Text
toTextEnglish (Location letter) = case letter of
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
  Y -> "Yangmingshan"
  Z -> "Lienchiang County"

-- | Generate a random 'Location'.
--
generate :: MonadRandom m => m Location
generate = Location <$> Letter.generate
