module Stellar.Core.Types.Sha256 where

import           Control.Monad           (fail)
import           Control.Newtype         (Newtype, pack, unpack)
import           Data.Aeson              (FromJSON, ToJSON, Value (..),
                                          parseJSON, toJSON, withText)
import           Data.Binary.Extended
import           Data.Binary.Get         (getByteString, label)
import           Data.ByteArray.Encoding (Base (Base16), convertFromBase,
                                          convertToBase)
import qualified Data.ByteString.Char8   as BS (pack)
import           Data.StaticText         (Static)
import qualified Data.StaticText         as S
import           Data.Text.Encoding      (decodeUtf8)
import           Prelude                 (show)
import           Protolude               hiding (show)
import           Text.Read.Extended      ((<++))
import qualified Text.Read.Extended      as R

newtype Sha256
  = Sha256 (Static ByteString 32)
  deriving (Eq)

instance Newtype Sha256 (Static ByteString 32) where
  pack = Sha256
  unpack (Sha256 s) = s

instance Show Sha256 where
  show = toS . printHashHex

instance Read Sha256 where
  readPrec = do
    let hexChars = R.lowerAz <++ R.upperAz <++ R.digit
    sha <- mfilter ((== 64) . length) $ many hexChars
    convertFromBase Base16 (BS.pack sha) >>=
      (note "Sha 356 ByteString length != 32" . S.create)
      & either (fail . show) (pure . Sha256)
  readListPrec = R.readListPrecDefault

instance ToJSON Sha256 where
  toJSON = String . printHashHex

instance FromJSON Sha256 where
  parseJSON = withText "Sha256" $
    either (fail . show) pure . readEither . toS

instance Binary Sha256 where
  put = putFixLenByteString 32 . S.unwrap . unpack
  get = label "Sha256" $ pack . S.unsafeCreate <$> getByteString 32

printHashHex :: Sha256 -> Text
printHashHex = decodeUtf8 . convertToBase Base16 . S.unwrap . unpack
