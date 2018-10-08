{-# LANGUAGE StrictData      #-}

module Stellar.Client.Types
  ( Account (..)
  , AccountId
  , unsafeAccountId
  , AccountFlags (..)
  , Balance (..)
  , Trustline (..)
  , isTrustline
  , Cursor
  , SortOrder (..)
  , Liabilities (..)
  , TransactionDetails (..)
  , TransactionId (..)
  , Thresholds (..)
  , Ledger (..)
  ) where

import Control.Monad    (fail)
import Control.Newtype
import Data.Aeson.Types
import Data.Char        (isDigit)
import Protolude
import Stellar
import Web.HttpApiData  (ToHttpApiData (..))

newtype AccountId
  = AccountId Text
  deriving (Eq, Show, FromJSON, ToJSON)

instance StringConv AccountId Text where
  strConv _ (AccountId t) = t

unsafeAccountId :: Text -> AccountId
unsafeAccountId = AccountId

data AccountFlags
  = AccountFlags
   { _authRequired  :: Bool
   , _authRevocable :: Bool
   } deriving (Eq, Show)

instance FromJSON AccountFlags where
  parseJSON = withObject "Account Flag" $ \o -> do
    _authRequired  <- o .: "auth_required"
    _authRevocable <- o .: "auth_revocable"
    return $ AccountFlags {..}


data Liabilities
  = Liabilities
  { _buying  :: NonNegativeInt64
  , _selling :: NonNegativeInt64
  } deriving (Eq, Show)


data Trustline
  = Trustline
  { _asset :: NonNativeAsset
  , _limit :: NonNegativeInt64
  } deriving (Eq, Show)

instance FromJSON Trustline where
  parseJSON = withObject "Trustline" $ \o -> do
    _limit <- parseNonNegativeInt64 o "limit"
    _asset <- NonNativeAsset <$> o .: "asset_code" <*> o .: "asset_issuer"
    return $ Trustline {..}

type NonNativeBalance = (Trustline, NonNegativeInt64)

data Balance
  = Balance
  { _liabilities :: Liabilities
  , _assetLimit  :: Either Stroop NonNativeBalance
  } deriving (Eq, Show)

isTrustline :: Balance -> Bool
isTrustline Balance {..} = isRight _assetLimit

instance FromJSON Balance where
  parseJSON = withObject "Balance" $ \o -> do
    _liabilities <- Liabilities
      <$> parseNonNegativeInt64 o "buying_liabilities"
      <*> parseNonNegativeInt64 o "selling_liabilities"
    _assetLimit <- o .: "asset_type" >>= \case
      PreciseAssetTypeNative -> Left . Stroop <$> parseInt64 o "balance"
      PreciseAssetTypeCreditAlphanum4  -> Right <$> parseNonNativeBalance o
      PreciseAssetTypeCreditAlphanum12 -> Right <$> parseNonNativeBalance o
    return Balance {..}
--
-- instance ToJSON Balance where
--   toJSON Balance {..} =

parseNonNativeBalance :: Object  -> Parser NonNativeBalance
parseNonNativeBalance o = (,)
  <$> parseJSON (Object o)
  <*> parseNonNegativeInt64 o "balance"

parseInt64 :: Object -> Text -> Parser Int64
parseInt64 o key = do
  numStr <- o .: key
  maybe (fail ("Invalid " <> toS key)) pure $ readMaybe $ filter isDigit numStr

parseNonNegativeInt64 :: Object -> Text -> Parser NonNegativeInt64
parseNonNegativeInt64 = notImplemented

data Thresholds
  = Thresholds
  { _lowThreshold    :: Threshold
  , _mediumThreshold :: Threshold
  , _highThreshold   :: Threshold
  } deriving (Eq, Show)

instance FromJSON Thresholds where
  parseJSON = withObject "Thresholds" $ \o -> Thresholds
    <$> o .: "low_threshold"
    <*> o .: "med_threshold"
    <*> o .: "high_threshold"

instance ToJSON Thresholds where
  toJSON Thresholds {..} = object
    [ "low_threshold"  .= _lowThreshold
    , "med_threshold"  .= _mediumThreshold
    , "high_threshold" .= _highThreshold
    ]


data Account
  = Account
  { _id             :: AccountId
  , _publicKey      :: PublicKey
  , _sequenceNumber :: SequenceNumber
  , _subentryCount  :: Word32
  , _thresholds     :: Thresholds
  , _balances       :: [Balance]
  , _flags          :: AccountFlags
  , _signers        :: [Signer]
  , _dataValues     :: Map Text DataValue
  } deriving (Eq, Show, Generic)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \o -> do
    _id             <- o .: "id"
    _publicKey      <- o .: "account_id"
    _sequenceNumber <- o .: "sequence"
    _subentryCount  <- o .: "subentry_count"
    _balances       <- o .: "balances"
    _thresholds     <- o .: "thresholds"
    _flags          <- o .: "flags"
    _signers        <- o .: "signers"
    _dataValues     <- o .: "data"
    return Account {..}

-- instance ToJSON Account where
--   toJSON Account {..} = object
--     [ "id" .= _id
--     , "account_id" .= _publicKey
--     , "sequence" .= _sequenceNumber
--     , "subentry_count" .= _subentryCount
--     , "balances" .= _balances
--     , "thresholds" .= _thresholds
--     , "signers" .= _signers
--     , "data" .= _dataValues
--     ]



newtype Cursor
  = Cursor Text
  deriving (Eq, Show, FromJSON, ToHttpApiData)

instance Newtype Cursor Text where
  unpack (Cursor t) = t
  pack = Cursor

data SortOrder
  = Ascending | Descending
  deriving (Eq, Show)

instance ToHttpApiData SortOrder where
  toUrlPiece = \case
    Ascending -> "asc"
    Descending -> "desc"

newtype TransactionId
  = TransactionId Sha256
  deriving (Eq, Show, Read, FromJSON)

newtype Ledger
  = Ledger Word64
  deriving (Eq, Show, Read, FromJSON)

data TransactionDetails
  = TransactionDetails
  { _id :: TransactionId
  , _ledger :: Ledger
  -- TODO: other fields
  } deriving (Eq, Show, Generic)

instance FromJSON TransactionDetails where
  parseJSON = withObject "TransactionDetails" $ \o -> do
    _id <- o .: "id"
    _ledger <- o .: "ledger"
    return TransactionDetails {..}
