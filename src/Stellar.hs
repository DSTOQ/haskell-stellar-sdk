{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}

module Stellar
  ( VarLen
  , unVarLen
  , FixLen
  , unFixLen
  , PublicKeyType (..)
  , PublicKey (..)
  , SignerKeyType (..)
  , signerKeyType
  , SignerKey (..)
  , Threshold (..)
  , AssetCode4 (..)
  , AssetCode12 (..)
  , AssetType (..)
  , assetType
  , Asset (..)
  , Price (..)
  , Fee (..)
  , SequenceNumber (..)
  , Hash (..)
  , MemoType (..)
  , Memo (..)
  , Signer (..)
  , CreateAccountOp (..)
  , PaymentOp (..)
  , PathPaymentOp (..)
  , OfferId (..)
  , ManageOfferOp (..)
  , CreatePassiveOfferOp (..)
  , HomeDomain (..)
  , SetOptionsOp (..)
  , ChangeTrustOp (..)
  , AllowTrustOp (..)
  , DataValue
  , mkDataValue
  , unDataValue
  , ManageDataOp (..)
  , OperationType (..)
  , operationType
  , OperationBody (..)
  , Operation (..)
  , TimeBounds (..)
  , Transaction (..)
  , SignatureHint (..)
  , Signature (..)
  , DecoratedSignature (..)
  , TransactionEnvelope (..)
  ) where

import           Control.Monad        (fail)
import           Data.Binary.Extended
import           Data.Binary.Get      (Get)
import           Data.ByteString      as BS
import           Data.LargeWord       (Word256, Word96)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Word            (Word32)
import           Protolude            hiding (get, put)
import           Stellar.Internal

data PublicKeyType
  = PublicKeyTypeEd25519
  deriving (Eq, Show, Enum, Bounded)

instance Binary PublicKeyType where
  get = getEnum
  put = putEnum


newtype PublicKey
  = PublicKeyEd25519
  { unPublicKeyEd25519 :: Word256
  } deriving (Eq, Show)

instance Binary PublicKey where
  put pk = put PublicKeyTypeEd25519
        >> put (unPublicKeyEd25519 pk)
  get = get >>= \case PublicKeyTypeEd25519 -> PublicKeyEd25519 <$> get


data SignerKeyType
  = SignerKeyTypeEd25519
  | SignerKeyTypePreAuthTx
  | SignerKeyTypeHashX
  deriving (Eq, Show, Enum, Bounded)

instance Binary SignerKeyType where
  get = getEnum
  put = putEnum

signerKeyType :: SignerKey -> SignerKeyType
signerKeyType = \case
  SignerKeyEd25519 _   -> SignerKeyTypeEd25519
  SignerKeyPreAuthTx _ -> SignerKeyTypePreAuthTx
  SignerKeyHashX _     -> SignerKeyTypeHashX


data SignerKey
  = SignerKeyEd25519 Word256
  | SignerKeyPreAuthTx Word256
  | SignerKeyHashX Word256
  deriving (Eq, Show)

instance Binary SignerKey where
  get = do
    kt <- get
    get <&> case kt of
      SignerKeyTypeEd25519   -> SignerKeyEd25519
      SignerKeyTypePreAuthTx -> SignerKeyPreAuthTx
      SignerKeyTypeHashX     -> SignerKeyHashX
  put (SignerKeyEd25519 w256)   = put SignerKeyTypeEd25519   >> put w256
  put (SignerKeyPreAuthTx w256) = put SignerKeyTypePreAuthTx >> put w256
  put (SignerKeyHashX w256)     = put SignerKeyTypeHashX     >> put w256


newtype Threshold
  = Threshold
  { unThreshold :: Word32
  } deriving (Eq, Show, Binary)


newtype AssetCode4
  = AssetCode4
  { unAssetCode4 :: Word32
  } deriving (Eq, Show, Binary)


newtype AssetCode12
  = AssetCode12
  { unAssetCode12 :: Word96
  } deriving (Eq, Show, Binary)


data AssetType
  = AssetTypeNative
  | AssetTypeCreditAlphanum4
  | AssetTypeCreditAlphanum12
  deriving (Eq, Show, Enum, Bounded)

instance Binary AssetType where
  get = getEnum
  put = putEnum

assetType :: Asset -> AssetType
assetType = \case
  AssetNative -> AssetTypeNative
  AssetCreditAlphanum4 _ _  -> AssetTypeCreditAlphanum4
  AssetCreditAlphanum12 _ _ -> AssetTypeCreditAlphanum12


data Asset
  = AssetNative
  | AssetCreditAlphanum4 AssetCode4 PublicKey
  | AssetCreditAlphanum12 AssetCode12 PublicKey
  deriving (Eq, Show)

instance Binary Asset where
  get = get >>= \case
    AssetTypeNative           -> pure AssetNative
    AssetTypeCreditAlphanum4  -> AssetCreditAlphanum4 <$> get <*> get
    AssetTypeCreditAlphanum12 -> AssetCreditAlphanum12 <$> get <*> get
  put AssetNative = put AssetTypeNative
  put (AssetCreditAlphanum4 code pk) =
    put AssetTypeCreditAlphanum4  >> put code >> put pk
  put (AssetCreditAlphanum12 code pk) =
    put AssetTypeCreditAlphanum12 >> put code >> put pk


data Price
  = Price
  { numerator   :: Int32
  , denominator :: Int32
  } deriving (Eq, Show, Generic)

instance Binary Price


newtype Fee
  = Fee
  { unFee :: Word32
  } deriving (Eq, Show, Binary)


newtype SequenceNumber
  = SequenceNumber
  { unSequenceNumber :: Int64
  } deriving (Eq, Show, Binary)


newtype Hash
  = Hash
  { unHash :: Word256
  } deriving (Eq, Show, Binary)


data MemoType
  = MemoTypeNone
  | MemoTypeText
  | MemoTypeId
  | MemoTypeHash
  | MemoTypeReturn
  deriving (Eq, Show, Enum, Bounded)

instance Binary MemoType where
  get = getEnum
  put = putEnum


data Memo
  = MemoNone
  | MemoText (VarLen 28 ByteString)
  | MemoId Word64
  | MemoHash Hash -- the hash of what to pull from the content server
  | MemoReturn Hash -- the hash of the tx you are rejecting
  deriving (Eq, Show)

instance Binary Memo where
  put MemoNone       = put MemoTypeNone
  put (MemoText t)   = put MemoTypeText >> put t
  put (MemoId i)     = put MemoTypeId >> put i
  put (MemoHash h)   = put MemoTypeHash >> put h
  put (MemoReturn h) = put MemoTypeReturn >> put h
  get = do
    t <- get
    case t of
      MemoTypeNone   -> pure MemoNone
      MemoTypeText   -> MemoText <$> get
      MemoTypeId     -> MemoId <$> get
      MemoTypeHash   -> MemoHash <$> get
      MemoTypeReturn -> MemoReturn <$> get


data Signer
  = Signer
  { key    :: SignerKey
  , weight :: Word32
  } deriving (Eq, Show, Generic)

instance Binary Signer


data CreateAccountOp
  = CreateAccountOp
  { destination     :: PublicKey
  , startingBalance :: Int64
  } deriving (Eq, Show, Generic)

instance Binary CreateAccountOp


data PaymentOp
  = PaymentOp
  { destination :: PublicKey
  , asset       :: Asset
  , amount      :: Int64
  } deriving (Eq, Show, Generic)

instance Binary PaymentOp


data PathPaymentOp
  = PathPaymentOp
  { sendAsset   :: Asset
  , sendMax     :: Int64
  , destination :: PublicKey
  , destAsset   :: Asset
  , destAmount  :: Int64
  , path        :: [Asset]
  } deriving (Eq, Show, Generic)

instance Binary PathPaymentOp


newtype OfferId
  = OfferId
  { unOfferId :: Word64
  } deriving (Eq, Show, Binary)


data ManageOfferOp
  = ManageOfferOp
  { selling :: Asset
  , buying  :: Asset
  , amount  :: Int64
  , price   :: Price
  , offerId :: OfferId
  } deriving (Eq, Show, Generic)

instance Binary ManageOfferOp


data CreatePassiveOfferOp
  = CreatePassiveOfferOp
  { selling :: Asset
  , buying  :: Asset
  , amount  :: Int64
  , price   :: Price
  } deriving (Eq, Show, Generic)

instance Binary CreatePassiveOfferOp


newtype HomeDomain
  = HomeDomain
  { unHomeDomain :: VarLen 32 Text
  } deriving (Eq, Show, Binary)


data SetOptionsOp
  = SetOptionsOp
  { inflationDest   :: Maybe PublicKey    -- inflation destination
  , clearFlags      :: Maybe Word32       -- which flags to clear
  , setFlags        :: Maybe Word32       -- which flags to set
  , masterWeight    :: Maybe Word32       -- weight of the master account
  , lowThreshold    :: Maybe Threshold
  , mediumThreshold :: Maybe Threshold
  , highThreshold   :: Maybe Threshold
  , homeDomain      :: Maybe HomeDomain
  , signer          :: Maybe Signer
  } deriving (Eq, Show)

instance Binary SetOptionsOp where
  put op = do
    op & put . Padded . inflationDest
    op & put . Padded . clearFlags
    op & put . Padded . setFlags
    op & put . Padded . masterWeight
    op & put . Padded . lowThreshold
    op & put . Padded . mediumThreshold
    op & put . Padded . highThreshold
    op & put . Padded . homeDomain
    op & put . Padded . signer
  get = SetOptionsOp
    <$> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get


data ChangeTrustOp
  = ChangeTrustOp
  { line  :: Asset
  , limit :: Maybe Int64   -- limit, Nothing deletes the trust line
  } deriving (Eq, Show)

instance Binary ChangeTrustOp where
  put op = do
    op & put . line
    op & put . Padded . limit
  get = ChangeTrustOp <$> get <*> fmap unPadded get


data AllowTrustOp
  = AllowTrustOp
  { trustor   :: PublicKey
  , asset     :: Either AssetCode4 AssetCode12
  , authorize :: Bool
  } deriving (Eq, Show)

instance Binary AllowTrustOp where
  put op = do
    op & put . trustor
    either (put . (AssetTypeCreditAlphanum4,))
           (put . (AssetTypeCreditAlphanum12,))
           $ asset (op :: AllowTrustOp)
    op & put . Padded . authorize
  get = do
    trustor <- get
    asset <- get >>= \case
      AssetTypeNative -> fail "Can't allow trust for a native asset"
      AssetTypeCreditAlphanum4  -> fmap Left  (get :: Get AssetCode4)
      AssetTypeCreditAlphanum12 -> fmap Right (get :: Get AssetCode12)
    authorize <- fmap unPadded get
    pure $ AllowTrustOp trustor asset authorize


data ManageDataOp
  = ManageDataOp
  { dataName  :: VarLen 64 Text
  , dataValue :: Maybe DataValue
  } deriving (Eq, Show)

instance Binary ManageDataOp where
  put op = do
    op & put . dataName
    op & put . Padded . dataValue
  get = ManageDataOp <$> get <*> fmap unPadded get


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
  get = getEnum
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
  get = get >>= \case
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
  { sourceAccount :: Maybe PublicKey
  , body          :: OperationBody
  } deriving (Eq, Show)

instance Binary Operation where
  put op = do
    put $ Padded $ sourceAccount (op :: Operation)
    put $ body op
  get = Operation
    <$> fmap unPadded get
    <*> get


data TimeBounds
  = TimeBounds
  { minTime :: Word64
  , maxTime :: Maybe Word64 -- 0 here means no maxTime
  } deriving (Eq, Show)

instance Binary TimeBounds where
  get = do
    mn <- get
    mx <- get
    pure $ TimeBounds mn $ if mx == 0 then Nothing else Just mx
  put (TimeBounds mn mx) = put mn >> put (fromMaybe 0 mx)


data Transaction
  = Transaction
  { sourceAccount :: PublicKey
  , fee           :: Fee
  , seqNum        :: SequenceNumber
  , timeBounds    :: Maybe TimeBounds
  , memo          :: Memo
  , operations    :: VarLen 100 (NonEmpty Operation)
  } deriving (Eq, Show)

instance Binary Transaction where
  put tx = do
    put $ sourceAccount (tx :: Transaction)
    tx & put . fee
    tx & put . seqNum
    tx & put . Padded . timeBounds
    tx & put . memo
    tx & put . operations
  get = Transaction
    <$> get               -- sourceAccount
    <*> get               -- fee
    <*> get               -- seqNum
    <*> fmap unPadded get -- timeBounds
    <*> get               -- memo
    <*> get               -- operations


newtype SignatureHint
  = SignatureHint
  { unSignatureHint :: Word32
  } deriving (Eq, Show, Binary)


newtype Signature
  = Signature
  { unSignature :: FixLen 256 ByteString
  } deriving (Eq, Show, Binary)


data DecoratedSignature
  = DecoratedSignature
  { signatureHint :: SignatureHint
  , signature     :: Signature
  } deriving (Eq, Show, Generic)

instance Binary DecoratedSignature


data TransactionEnvelope
  = TransactionEnvelope
  { transaction :: Transaction
  , signatures  :: [DecoratedSignature]
  } deriving (Eq, Show, Generic)

instance Binary TransactionEnvelope
