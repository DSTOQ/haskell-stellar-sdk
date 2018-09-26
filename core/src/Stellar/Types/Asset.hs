{-# LANGUAGE StrictData #-}

module Stellar.Types.Asset
  ( AssetCode
  , makeAssetCode
  , unsafeAssetCode
  , putAssetCode
  , getAssetCode4
  , getAssetCode12
  , PreciseAssetType (..)
  , AssetType (..)
  , assetType
  , Asset (..)
  ) where

import           Control.Monad        (fail)
import           Control.Newtype      (Newtype, pack, unpack)
import           Data.Aeson
import           Data.Binary.Extended
import           Data.Binary.Get      (Get, getByteString, label)
import qualified Data.ByteString      as BS
import           Data.Char            (isAlphaNum, isAscii)
import qualified Data.Text            as T
import           Protolude            hiding (get, put, show)
import           Stellar.Types.Key
import Text.Read.Extended (Lexeme (Ident), (<++))

import qualified Text.Read.Extended   as R
import           Text.Show            (show)

newtype AssetCode
  = AssetCode
  { _assetCode :: ByteString
  } deriving (Eq)

instance Newtype AssetCode ByteString where
  pack = AssetCode
  unpack = _assetCode

instance Show AssetCode where
  show (AssetCode c) = toS c

instance Read AssetCode where
  readPrec = do
    s <- mfilter (\cs -> let l = length cs in l >= 1 && l <= 12)
       $ many $ R.upperAz <++ R.lowerAz <++ R.digit
    pure $ AssetCode $ toS s
  readListPrec = R.readListPrecDefault

instance ToJSON AssetCode where
  toJSON = String . toS . unpack

instance FromJSON AssetCode where
  parseJSON = withText "Asset Code" $
    either (fail . show) pure . readEither . toS

makeAssetCode :: Text -> Maybe AssetCode
makeAssetCode t
  | l > 0 && l < 13 && isAsciiAlphaNum t  = Just $ AssetCode $ toS t
  | otherwise = Nothing
  where
    l = T.length t
    isAsciiAlphaNum = T.all $ getAll . mappend allAlphaNum allAscii
    allAlphaNum = fmap All isAlphaNum
    allAscii = fmap All isAscii

unsafeAssetCode :: ByteString -> AssetCode
unsafeAssetCode = AssetCode

assetCodePreciseAssetType :: AssetCode -> PreciseAssetType
assetCodePreciseAssetType (AssetCode bs)
  | BS.length bs < 5 = PreciseAssetTypeCreditAlphanum4
  | otherwise = PreciseAssetTypeCreditAlphanum12

putAssetCode :: AssetCode -> Put
putAssetCode code@(AssetCode bs) = do
  let preciseAssetType = assetCodePreciseAssetType code
  put preciseAssetType
  case preciseAssetType of
    PreciseAssetTypeCreditAlphanum4 -> putSizedByteString 4
    _                               -> putSizedByteString 12
    where
      putSizedByteString size =
        let zeroSuffixed = bs <> BS.replicate (padding size (BS.length bs)) 0
        in putFixLenByteString size $ BS.take size zeroSuffixed

data AssetType
  = AssetTypeNative
  | AssetTypeCreditAlphanum
  deriving (Eq, Enum, Bounded)

data PreciseAssetType
  = PreciseAssetTypeNative
  | PreciseAssetTypeCreditAlphanum4
  | PreciseAssetTypeCreditAlphanum12
  deriving (Eq, Enum, Bounded)

printPreciseAssetType :: PreciseAssetType -> Text
printPreciseAssetType = \case
  PreciseAssetTypeNative -> "native"
  PreciseAssetTypeCreditAlphanum4 -> "credit_alphanum4"
  PreciseAssetTypeCreditAlphanum12 -> "credit_alphanum12"

instance Show PreciseAssetType where
  show = toS . printPreciseAssetType

instance Read PreciseAssetType where
  readPrec = R.lexP >>= \case
    Ident "native"            -> pure PreciseAssetTypeNative
    Ident "credit_alphanum4"  -> pure PreciseAssetTypeCreditAlphanum4
    Ident "credit_alphanum12" -> pure PreciseAssetTypeCreditAlphanum12
    t                         -> fail $ "Invalid PreciseAssetType: " <> show t
  readListPrec = R.readListPrecDefault

instance ToJSON PreciseAssetType where
  toJSON = String . printPreciseAssetType

instance FromJSON PreciseAssetType where
  parseJSON = withText "PreciseAssetType" $
    either (fail . show) pure . readEither . toS

instance Binary PreciseAssetType where
  get = label "PreciseAssetType" getEnum
  put = putEnum

assetType :: Asset -> AssetType
assetType = \case
  AssetNative -> AssetTypeNative
  AssetCreditAlphanum _ _  -> AssetTypeCreditAlphanum


data Asset
  = AssetNative
  | AssetCreditAlphanum AssetCode PublicKey
  deriving (Eq, Show)

instance Binary Asset where
  put AssetNative                   = put PreciseAssetTypeNative
  put (AssetCreditAlphanum code pk) = putAssetCode code >> put pk

  get = label "Asset" $ get >>= \case
    PreciseAssetTypeNative ->
      pure AssetNative
    PreciseAssetTypeCreditAlphanum4 ->
      AssetCreditAlphanum <$> getAssetCode4 <*> get
    PreciseAssetTypeCreditAlphanum12 ->
      AssetCreditAlphanum <$> getAssetCode12 <*> get

getAssetCode4 :: Get AssetCode
getAssetCode4 = label "AssetCode (4)"
  $ AssetCode . BS.takeWhile (/= 0) <$> getByteString 4

getAssetCode12 :: Get AssetCode
getAssetCode12 = label "AssetCode (12)"
  $ AssetCode . BS.takeWhile (/= 0) <$> getByteString 12
