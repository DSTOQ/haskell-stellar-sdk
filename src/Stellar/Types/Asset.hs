{-# LANGUAGE StrictData #-}

module Stellar.Types.Asset
  ( AssetCode
  , makeAssetCode
  , unsafeAssetCode
  , putAssetCode
  , getAssetCode4
  , getAssetCode12
  , XdrAssetType (..)
  , AssetType (..)
  , assetType
  , Asset (..)
  ) where

import           Data.Binary.Extended
import           Data.Binary.Get        (Get, label, getByteString)
import qualified Data.ByteString        as BS
import           Protolude              hiding (get, put, show)
import           Stellar.Types.Key
import qualified Data.Text as T
import Data.Char (isAscii, isAlphaNum )


newtype AssetCode
  = AssetCode
  { _assetCode :: ByteString
  } deriving (Eq, Show)

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

assetCodeXdrAssetType :: AssetCode -> XdrAssetType
assetCodeXdrAssetType (AssetCode bs)
  | BS.length bs < 5 = XdrAssetTypeCreditAlphanum4
  | otherwise = XdrAssetTypeCreditAlphanum12

putAssetCode :: AssetCode -> Put
putAssetCode code@(AssetCode bs) = do
  let xdrAssetType = assetCodeXdrAssetType code
  put xdrAssetType
  case xdrAssetType of
    XdrAssetTypeCreditAlphanum4 -> putSizedByteString 4
    _                           -> putSizedByteString 12
    where
      putSizedByteString size =
        let zeroSuffixed = bs <> BS.replicate (padding size (BS.length bs)) 0
        in putFixLenByteString size $ BS.take size zeroSuffixed

data AssetType
  = AssetTypeNative
  | AssetTypeCreditAlphanum
  deriving (Eq, Show, Enum, Bounded)

data XdrAssetType
  = XdrAssetTypeNative
  | XdrAssetTypeCreditAlphanum4
  | XdrAssetTypeCreditAlphanum12
  deriving (Eq, Show, Enum, Bounded)

instance Binary XdrAssetType where
  get = label "XdrAssetType" getEnum
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
  put AssetNative = put XdrAssetTypeNative
  put (AssetCreditAlphanum code pk) = putAssetCode code >> put pk

  get = label "Asset" $ get >>= \case
    XdrAssetTypeNative ->
      pure AssetNative
    XdrAssetTypeCreditAlphanum4 ->
      AssetCreditAlphanum <$> getAssetCode4 <*> get
    XdrAssetTypeCreditAlphanum12 ->
      AssetCreditAlphanum <$> getAssetCode12 <*> get

getAssetCode4 :: Get AssetCode
getAssetCode4 = label "AssetCode (4)"
  $ AssetCode . BS.takeWhile (/= 0) <$> getByteString 4

getAssetCode12 :: Get AssetCode
getAssetCode12 = label "AssetCode (12)"
  $ AssetCode . BS.takeWhile (/= 0) <$> getByteString 12
