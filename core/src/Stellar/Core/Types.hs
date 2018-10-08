{-# LANGUAGE StrictData      #-}

module Stellar.Core.Types
  ( module Stellar.Core.Types.Asset
  , module Stellar.Core.Types.Key
  , module Stellar.Core.Types.Lumen
  , AllowTrustOp (..)
  , ChangeTrustOp (..)
  , CreateAccountOp (..)
  , CreatePassiveOfferOp (..)
  , DataValue (..)
  , DecoratedSignature (..)
  , EnvelopeType (..)
  , Fee (..)
  , ManageDataOp (..)
  , ManageOfferOp (..)
  , Memo (..)
  , MemoType (..)
  , Network (..)
  , OfferId (..)
  , newOfferId
  , Operation (..)
  , OperationBody (..)
  , OperationType (..)
  , Sha256 (..)
  , HomeDomain (..)
  , PathPaymentOp (..)
  , PaymentOp (..)
  , NonNegativeInt64 (..)
  , Price (..)
  , SetOptionsOp (..)
  , SequenceNumber (..)
  , unsafeSequenceNumber
  , bumpSequenceNumber
  , printSequenceNumber
  , Signature (..)
  , Signer (..)
  , TimeBounds (..)
  , Threshold (..)
  , Transaction (..)
  , TransactionEnvelope (..)
  ) where

import           Control.Monad            (fail)
import           Control.Newtype          (Newtype, pack, unpack)
import qualified Crypto.PubKey.Ed25519    as ED
import           Data.Aeson               (FromJSON, ToJSON, Value (..),
                                           parseJSON, toJSON, withObject,
                                           withText, (.:))
import           Data.Binary.Extended
import           Data.Binary.Get          (getInt64be, getWord32be, getWord64be,
                                           label, skip)
import           Data.Binary.Put          (putWord32be)
import qualified Data.ByteArray           as BA
import           Data.ByteArray.Encoding  (Base (Base64), convertFromBase,
                                           convertToBase)
import qualified Data.ByteString.Extended as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.String              as S
import qualified Data.Text.Encoding       as TE
import           Data.Word.Extended       (Word32)
import           GHC.Exts                 (fromList)
import           Protolude                hiding (get, put, show)
import           Refined
import           Stellar.Core.Types.Asset
import           Stellar.Core.Types.Internal
import           Stellar.Core.Types.Key
import           Stellar.Core.Types.Lumen
import           Stellar.Core.Types.Sha256
import           Text.Read                (readListPrec, readListPrecDefault,
                                           readPrec)
import           Text.Show                (show)

newtype NonNegativeInt64
  = NonNegativeInt64 (Refined NonNegative Int64)
  deriving (Eq, Show, Read)

instance Bounded NonNegativeInt64 where
  minBound = NonNegativeInt64 $ unsafeRefine 0
  maxBound = NonNegativeInt64 $ unsafeRefine maxBound

instance Ord NonNegativeInt64 where
  compare a b = unrefine (unpack a) `compare` unrefine (unpack b)

instance Newtype NonNegativeInt64 (Refined NonNegative Int64) where
  pack = NonNegativeInt64
  unpack (NonNegativeInt64 r) = r

instance Binary NonNegativeInt64 where
  put = put . unrefine . unpack
  get = label "NonNegativeInt64" $ getInt64be >>= refineFail <&> pack


newtype Threshold
  = Threshold
  { _threshold :: Word32
  } deriving (Eq, Show, FromJSON, ToJSON)

instance Newtype Threshold Word32 where
  pack = Threshold
  unpack = _threshold

instance Binary Threshold where
  get = label "Threshold" $ Threshold <$> get
  put = put . _threshold

data Price
  = Price
  { _numerator   :: Int32
  , _denominator :: Int32
  } deriving (Eq, Show, Generic)

instance Binary Price


newtype Fee
  = FeeStroops
  { _feeStroops :: Word32
  } deriving (Eq, Show, Read, Binary)

instance Newtype Fee Word32 where
  pack = FeeStroops
  unpack = _feeStroops


newtype SequenceNumber
  = SequenceNumber
  { _sequenceNumber :: NonNegativeInt64
  } deriving (Eq, Ord, Binary)

instance ToJSON SequenceNumber where
  toJSON = String . printSequenceNumber

instance FromJSON SequenceNumber where
  parseJSON = withText "SequenceNumber" $
    either (fail . show) pure . readEither . toS

instance Show SequenceNumber where
  show = printSequenceNumber

instance Read SequenceNumber where
  readPrec = do
    i <- readPrec
    r <- either (const $ fail ". show") pure $ refine i
    pure $ pack $ pack r
  readListPrec = readListPrecDefault

instance Newtype SequenceNumber NonNegativeInt64 where
  pack = SequenceNumber
  unpack = _sequenceNumber

unsafeSequenceNumber :: Int64 -> SequenceNumber
unsafeSequenceNumber = pack . pack . unsafeRefine

bumpSequenceNumber :: Int64 -> SequenceNumber -> SequenceNumber
bumpSequenceNumber i = unsafeSequenceNumber . (+ i) . unrefine . unpack . unpack

printSequenceNumber :: StringConv S.String a => SequenceNumber -> a
printSequenceNumber = toS . show . unrefine . unpack . unpack

data MemoType
  = MemoTypeNone
  | MemoTypeText
  | MemoTypeId
  | MemoTypeHash
  | MemoTypeReturn
  deriving (Eq, Show, Enum, Bounded)

instance Binary MemoType where
  get = label "MemoType" getEnum
  put = putEnum


data Memo
  = MemoNone
  | MemoText ByteString
  | MemoId Word64
  | MemoHash Sha256
  | MemoReturn Sha256
  deriving (Eq, Show)

instance Binary Memo where
  put MemoNone       = put MemoTypeNone
  put (MemoText t)   = put MemoTypeText >> put (VarLen t :: VarLen 28 ByteString)
  put (MemoId i)     = put MemoTypeId >> put i
  put (MemoHash h)   = put MemoTypeHash >> put h
  put (MemoReturn h) = put MemoTypeReturn >> put h
  get = label "Memo" $ get >>= \case
      MemoTypeNone   -> pure MemoNone
      MemoTypeText   -> MemoText <$> getVarLen (Proxy :: Proxy 28)
      MemoTypeId     -> MemoId <$> get
      MemoTypeHash   -> MemoHash <$> get
      MemoTypeReturn -> MemoReturn <$> get


data Signer
  = Signer
  { _key    :: SignerKey
  , _weight :: Word32
  } deriving (Eq, Show, Generic)

instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> do
    signerType <- o .: "type"
    _key <- case signerType of
      SignerKeyTypeEd25519   -> do
        key <- o .: "key"
        pk <- either (fail "Invalid Signer Key") pure $ parsePublicKey key
        return $ SignerKeyEd25519 pk
      SignerKeyTypePreAuthTx -> SignerKeyPreAuthTx <$> o .: "key"
      SignerKeyTypeHashX     -> SignerKeyHashX <$> o .: "key"
    _weight <- o .: "weight"
    pure $ Signer {..}


instance Binary Signer


data CreateAccountOp
  = CreateAccountOp
  { _destination     :: PublicKey
  , _startingBalance :: Stroop
  } deriving (Eq, Show, Generic)

instance Binary CreateAccountOp


data PaymentOp
  = PaymentOp
  { _destination :: PublicKey
  , _asset       :: Asset
  , _amount      :: NonNegativeInt64
  } deriving (Eq, Show, Generic)

instance Binary PaymentOp


data PathPaymentOp
  = PathPaymentOp
  { _sendAsset   :: Asset
  , _sendMax     :: NonNegativeInt64
  , _destination :: PublicKey
  , _destAsset   :: Asset
  , _destAmount  :: NonNegativeInt64
  , _path        :: [Asset]
  } deriving (Eq, Show)

instance Binary PathPaymentOp where
  put op = do
    put $ _sendAsset op
    put $ _sendMax op
    put $ _destination (op :: PathPaymentOp)
    put $ _destAsset op
    put $ _destAmount op
    put (VarLen $ _path op :: VarLen 5 [Asset])
  get = label "PathPaymentOp" $ PathPaymentOp
    <$> get                          -- sendAsset
    <*> get                          -- sendMax
    <*> get                          -- destination
    <*> get                          -- destAsset
    <*> get                          -- destAmount
    <*> getVarLen (Proxy :: Proxy 5) -- path


newtype OfferId
  = OfferId
  { _offerId :: Word64
  } deriving (Eq, Show, Read, Binary)

instance Newtype OfferId Word64 where
  pack = OfferId
  unpack = _offerId

newOfferId :: OfferId
newOfferId = pack 0

data ManageOfferOp
  = ManageOfferOp
  { _selling :: Asset
  , _buying  :: Asset
  , _amount  :: NonNegativeInt64
  , _price   :: Price
  , _offerId :: OfferId
  } deriving (Eq, Show, Generic)

instance Binary ManageOfferOp


data CreatePassiveOfferOp
  = CreatePassiveOfferOp
  { _selling :: Asset
  , _buying  :: Asset
  , _amount  :: NonNegativeInt64
  , _price   :: Price
  } deriving (Eq, Show, Generic)

instance Binary CreatePassiveOfferOp


newtype HomeDomain
  = HomeDomain
  { _homeDomain :: Text
  } deriving (Eq, Show, Read)

instance Newtype HomeDomain Text where
  pack = HomeDomain
  unpack = _homeDomain

instance Binary HomeDomain where
  put (HomeDomain t) = put (VarLen t :: VarLen 32 Text)
  get = label "HomeDomain" $ HomeDomain <$> getVarLen (Proxy :: Proxy 32)


data SetOptionsOp
  = SetOptionsOp
  { _inflationDest   :: Maybe PublicKey    -- inflation destination
  , _clearFlags      :: Maybe Word32       -- which flags to clear
  , _setFlags        :: Maybe Word32       -- which flags to set
  , _masterWeight    :: Maybe Word32       -- weight of the master account
  , _lowThreshold    :: Maybe Threshold
  , _mediumThreshold :: Maybe Threshold
  , _highThreshold   :: Maybe Threshold
  , _homeDomain      :: Maybe HomeDomain
  , _signer          :: Maybe Signer
  } deriving (Eq, Show)

instance Binary SetOptionsOp where
  put op = do
    put $ Padded $ _inflationDest op
    put $ Padded $ _clearFlags op
    put $ Padded $ _setFlags op
    put $ Padded $ _masterWeight op
    put $ Padded $ _lowThreshold op
    put $ Padded $ _mediumThreshold op
    put $ Padded $ _highThreshold op
    put $ Padded $ _homeDomain (op :: SetOptionsOp)
    put $ Padded $ _signer op
  get = label "SetOptionsOp" $ SetOptionsOp
    <$> fmap unPadded get -- inflationDest
    <*> fmap unPadded get -- clearFlags
    <*> fmap unPadded get -- setFlags
    <*> fmap unPadded get -- masterWeight
    <*> fmap unPadded get -- lowThreshold
    <*> fmap unPadded get -- mediumThreshold
    <*> fmap unPadded get -- highThreshold
    <*> fmap unPadded get -- homeDomain
    <*> fmap unPadded get -- signer


data ChangeTrustOp
  = ChangeTrustOp
  { _line  :: Asset
  , _limit :: NonNegativeInt64
  } deriving (Eq, Show)

instance Binary ChangeTrustOp where
  put (ChangeTrustOp line limit) = put line >> put limit
  get = label "ChangeTrustOp" $ ChangeTrustOp <$> get <*> get


data AllowTrustOp
  = AllowTrustOp
  { _trustor   :: PublicKey
  , _asset     :: AssetCode
  , _authorize :: Bool
  } deriving (Eq, Show)

instance Binary AllowTrustOp where
  put op = do
    put $ _trustor op
    putAssetCode $ _asset (op :: AllowTrustOp)
    op & put . Padded . _authorize
  get = label "AllowTrustOp" $ do
    trustor <- get
    asset <- get >>= \case
      PreciseAssetTypeNative -> fail "Can't allow trust for a native asset"
      PreciseAssetTypeCreditAlphanum4 -> getAssetCode4
      PreciseAssetTypeCreditAlphanum12 -> getAssetCode12
    authorize <- fmap unPadded get
    pure $ AllowTrustOp trustor asset authorize


data ManageDataOp
  = ManageDataOp
  { _dataName  :: VarLen 64 Text
  , _dataValue :: Maybe DataValue
  } deriving (Eq, Show)

instance Binary ManageDataOp where
  put op = do
    op & put . _dataName
    op & put . Padded . _dataValue
  get = label "ManageDataOp" $ ManageDataOp
    <$> get
    <*> fmap unPadded get


data OperationType
  = OperationTypeCreateAccount
  | OperationTypePayment
  | OperationTypePathPayment
  | OperationTypeManageOffer
  | OperationTypeCreatePassiveOffer
  | OperationTypeSetOptions
  | OperationTypeChangeTrust
  | OperationTypeAllowTrust
  | OperationTypeAccountMerge
  | OperationTypeInflation
  | OperationTypeManageData
  | OperationTypeBumpSequence
  deriving (Eq, Show, Enum, Bounded)

instance Binary OperationType where
  get = label "OperationType" getEnum
  put = putEnum

operationType :: OperationBody -> OperationType
operationType op = case op of
  CreateAccount _      -> OperationTypeCreateAccount
  Payment _            -> OperationTypePayment
  PathPayment _        -> OperationTypePathPayment
  ManageOffer _        -> OperationTypeManageOffer
  CreatePassiveOffer _ -> OperationTypeCreatePassiveOffer
  SetOptions _         -> OperationTypeSetOptions
  ChangeTrust _        -> OperationTypeChangeTrust
  AllowTrust _         -> OperationTypeAllowTrust
  AccountMerge _       -> OperationTypeAccountMerge
  Inflation            -> OperationTypeInflation
  ManageData _         -> OperationTypeManageData
  BumpSequence _       -> OperationTypeBumpSequence

data OperationBody
  = CreateAccount CreateAccountOp
  | Payment PaymentOp
  | PathPayment PathPaymentOp
  | ManageOffer ManageOfferOp
  | CreatePassiveOffer CreatePassiveOfferOp
  | SetOptions SetOptionsOp
  | ChangeTrust ChangeTrustOp
  | AllowTrust AllowTrustOp
  | AccountMerge PublicKey
  | Inflation
  | ManageData ManageDataOp
  | BumpSequence SequenceNumber
  deriving (Eq, Show)

instance Binary OperationBody where
  put operation = do
    operation & put . operationType
    case operation of
      CreateAccount op      -> put op
      Payment op            -> put op
      PathPayment op        -> put op
      ManageOffer op        -> put op
      CreatePassiveOffer op -> put op
      SetOptions op         -> put op
      ChangeTrust op        -> put op
      AllowTrust op         -> put op
      AccountMerge pk       -> put pk
      Inflation             -> pure ()
      ManageData op         -> put op
      BumpSequence sn       -> put sn
  get = label "OperationBody" $ get >>= \case
    OperationTypeCreateAccount      -> CreateAccount      <$> get
    OperationTypePayment            -> Payment            <$> get
    OperationTypePathPayment        -> PathPayment        <$> get
    OperationTypeManageOffer        -> ManageOffer        <$> get
    OperationTypeCreatePassiveOffer -> CreatePassiveOffer <$> get
    OperationTypeSetOptions         -> SetOptions         <$> get
    OperationTypeChangeTrust        -> ChangeTrust        <$> get
    OperationTypeAllowTrust         -> AllowTrust         <$> get
    OperationTypeAccountMerge       -> AccountMerge       <$> get
    OperationTypeManageData         -> ManageData         <$> get
    OperationTypeBumpSequence       -> BumpSequence       <$> get
    OperationTypeInflation          -> pure Inflation


data Operation
  = Operation
  { _sourceAccount :: Maybe PublicKey
  , _body          :: OperationBody
  } deriving (Eq, Show)

instance Binary Operation where
  put op = do
    put $ Padded $ _sourceAccount (op :: Operation)
    put $ _body op
  get = label "Operation" $ Operation
    <$> fmap unPadded get    -- sourceAccount
    <*> get                  -- body


data TimeBounds
  = TimeBounds
  { _minTime :: Word64
  , _maxTime :: Word64
  } deriving (Eq, Show)

instance Binary TimeBounds where
  get = label "TimeBounds" $ TimeBounds
    <$> label "minTime" getWord64be
    <*> label "maxTime" getWord64be
  put (TimeBounds mn mx) = put mn >> put mx


data Transaction
  = Transaction
  { _sourceAccount :: PublicKey
  , _fee           :: Fee
  , _seqNum        :: SequenceNumber
  , _timeBounds    :: Maybe TimeBounds
  , _memo          :: Memo
  , _operations    :: [Operation]
  } deriving (Eq, Show)

instance Binary Transaction where
  put tx = do
    put $ _sourceAccount (tx :: Transaction)
    put $ _fee (tx :: Transaction)
    put $ _seqNum tx
    put $ Padded $ _timeBounds tx
    put $ _memo tx
    put $ (VarLen :: [Operation] -> VarLen 100 [Operation]) $ _operations tx
    putWord32be 0 -- ext
  get = label "Transaction" $ Transaction
    <$> get                            -- sourceAccount
    <*> get                            -- fee
    <*> get                            -- seqNum
    <*> fmap unPadded get              -- timeBounds
    <*> get                            -- memo
    <*> getVarLen (Proxy :: Proxy 100) -- operations
    <*  skip 4                         -- ext


newtype Signature
  = Signature
  { _signature :: ED.Signature
  } deriving (Eq)

instance Newtype Signature ED.Signature where
  pack = Signature
  unpack = _signature

instance Show Signature where
  show sig = "Signature {_signature = "
    <> show (BS.printByteStringBase16 $ BA.convert $ unpack sig)
    <> "}"

instance Binary Signature where
  put sig = put (VarLen (unpack sig) :: VarLen 64 ED.Signature)
  get = label "Signature" $ pack <$> getVarLen (Proxy :: Proxy 64)


data DecoratedSignature
  = DecoratedSignature
  { _hint      :: SignatureHint
  , _signature :: Signature
  } deriving (Eq, Show)

instance Binary DecoratedSignature where
  put (DecoratedSignature hint signature) =
    put hint >> put signature
  get = label "DecoratedSignature" $ DecoratedSignature
    <$> get -- hint
    <*> get -- signature


data EnvelopeType
  = EnvelopeTypeScp
  | EnvelopeTypeTx
  | EnvelopeTypeAuth
  deriving (Eq, Show, Enum, Bounded)

instance Binary EnvelopeType where
  -- This enum index starts from 1 unlike all other enum constants!
  get = label "EnvelopeType" $ getWord32be <&> toEnum . pred . fromIntegral
  put = putWord32be . fromIntegral . succ . fromEnum


data Network
  = Public
  | Testnet
  deriving (Eq, Show, Enum)

instance Binary Network where
  get = label "Network" getEnum
  put = putEnum


data TransactionEnvelope
  = TransactionEnvelope
  { _transaction :: Transaction
  , _signatures  :: [DecoratedSignature]
  } deriving (Eq, Show)

instance Binary TransactionEnvelope where
  put (TransactionEnvelope transaction signatures) = do
    put transaction
    put $ (VarLen :: [DecoratedSignature] -> VarLen 20 [DecoratedSignature])
        signatures
  get = label "TransactionEnvelope" $ TransactionEnvelope
    <$> get
    <*> getVarLen (Proxy :: Proxy 20)

instance ToJSON TransactionEnvelope where
  toJSON txe = Object $ fromList [("transaction_envelope_xdr", String xdr)]
    where xdr = decodeUtf8 $ convertToBase Base64 $ BSL.toStrict $ encode txe

instance FromJSON TransactionEnvelope where
  parseJSON = withObject "Transaction Envelope" $ \o -> do
    xdrB64 <- o .: "transaction_envelope_xdr"
    let xdrB64bytes = TE.encodeUtf8 xdrB64
        xdrBytes = convertFromBase Base64 xdrB64bytes
    either (fail . show) pure (xdrBytes >>= parseBin)
    where
      parseBin = bimap thd3 thd3 . decodeOrFail . BSL.fromStrict
      thd3 :: (a, b, c) -> c
      thd3 (_, _, c) = c
