{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}

module Stellar.Types where

import           Control.Monad          (fail)
import qualified Crypto.Error           as CE
import qualified Crypto.PubKey.Ed25519  as ED
import           Crypto.Random.Types    (MonadRandom)
import           Data.Binary.Extended
import           Data.Binary.Get        (Get, getInt64be, label, skip)
import           Data.Binary.Put        (putWord32be)
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.LargeWord         (Word256, Word96)
import           Data.Word.Extended     (Word32, word32FromBytes, word32ToBytes)
import           Prelude                (String, show)
import           Protolude              hiding (get, put, show)
import           Stellar.Types.Internal


data KeyPair
  = KeyPair
  { _secretKey :: ED.SecretKey
  , _publicKey :: PublicKey
  , _hint      :: SignatureHint
  } deriving (Eq)

instance Show KeyPair where
  show (KeyPair sk pk h) = "KeyPair {"
    <> "_secretKey = " <> Prelude.show sk
    <> ", _publicKey = " <> showByteString (BA.convert pk)
    <> ", _hint = " <> Prelude.show h
    <> "}"

showByteString :: ByteString -> String
showByteString = Prelude.show . (toS :: ByteString -> String) . B16.encode

keyPair :: ED.SecretKey -> ED.PublicKey -> KeyPair
keyPair sk pk = KeyPair sk (PublicKeyEd25519 pk) hint
  where
    hint :: SignatureHint
    hint = SignatureHint $ word32FromBytes $ takeR 4 $ BA.unpack pk

    takeR :: Int -> [a] -> [a]
    takeR n l = go (drop n l) l
      where
        go :: [a] -> [a] -> [a]
        go [] r          = r
        go (_:xs) (_:ys) = go xs ys
        go _ []          = []

keyPair' :: ED.SecretKey -> KeyPair
keyPair' sk = keyPair sk (ED.toPublic sk)

generateKeyPair :: MonadRandom m => m KeyPair
generateKeyPair = keyPair' <$> ED.generateSecretKey


data PublicKeyType
  = PublicKeyTypeEd25519
  deriving (Eq, Show, Enum, Bounded)

instance Binary PublicKeyType where
  get = label "PublicKeyType" getEnum
  put = putEnum


newtype PublicKey
  = PublicKeyEd25519
  { _publicKeyEd25519 :: ED.PublicKey
  } deriving (Eq, BA.ByteArrayAccess)

instance Show PublicKey where
  show (PublicKeyEd25519 pk) =
    "PublicKeyEd25519 {_publicKeyEd25519 = "
    <> showByteString (BA.convert pk) <> "}"

instance Binary PublicKey where
  put (PublicKeyEd25519 edPk) = do
    put PublicKeyTypeEd25519
    let k :: FixLen 32 ByteString
        k = FixLen (BA.convert edPk)
    put k
  get = label "PublicKey"
      $ get >>= \case PublicKeyTypeEd25519 -> do
                        fl :: FixLen 32 ByteString <- get
                        let bs = unFixLen fl
                        key <- ED.publicKey bs & CE.onCryptoFailure (fail . show) pure
                        pure $ PublicKeyEd25519 key


data SignerKeyType
  = SignerKeyTypeEd25519
  | SignerKeyTypePreAuthTx
  | SignerKeyTypeHashX
  deriving (Eq, Show, Enum, Bounded)

instance Binary SignerKeyType where
  get = label "SignerKeyType" getEnum
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
  get = label "SignerKey" $ do
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
  { _threshold :: Word32
  } deriving (Eq, Show)

instance Binary Threshold where
  get = label "Threshold" $ Threshold <$> get
  put = put . _threshold


newtype AssetCode4
  = AssetCode4
  { _assetCode4 :: Word32
  } deriving (Eq, Show)

instance Binary AssetCode4 where
  get = label "AssetCode4" $ AssetCode4 <$> get
  put = put . _assetCode4


newtype AssetCode12
  = AssetCode12
  { _assetCode12 :: Word96
  } deriving (Eq, Show)

instance Binary AssetCode12 where
  get = label "AssetCode12" $ AssetCode12 <$> get
  put = put . _assetCode12


data AssetType
  = AssetTypeNative
  | AssetTypeCreditAlphanum4
  | AssetTypeCreditAlphanum12
  deriving (Eq, Show, Enum, Bounded)

instance Binary AssetType where
  get = label "AssetType" getEnum
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
  get = label "Asset" $ get >>= \case
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
  { _numerator   :: Int32
  , _denominator :: Int32
  } deriving (Eq, Show, Generic)

instance Binary Price


newtype Fee
  = Fee
  { _fee :: Word32
  } deriving (Eq, Show, Binary)


newtype SequenceNumber
  = SequenceNumber
  { _sequenceNumber :: Int64
  } deriving (Eq, Show, Binary)


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
  | MemoHash Hash
  | MemoReturn Hash
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

instance Binary Signer


data CreateAccountOp
  = CreateAccountOp
  { _destination     :: PublicKey
  , _startingBalance :: Int64
  } deriving (Eq, Show, Generic)

instance Binary CreateAccountOp


data PaymentOp
  = PaymentOp
  { _destination :: PublicKey
  , _asset       :: Asset
  , _amount      :: Int64
  } deriving (Eq, Show, Generic)

instance Binary PaymentOp


data PathPaymentOp
  = PathPaymentOp
  { _sendAsset   :: Asset
  , _sendMax     :: Int64
  , _destination :: PublicKey
  , _destAsset   :: Asset
  , _destAmount  :: Int64
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
  } deriving (Eq, Show, Binary)


data ManageOfferOp
  = ManageOfferOp
  { _selling :: Asset
  , _buying  :: Asset
  , _amount  :: Int64
  , _price   :: Price
  , _offerId :: OfferId
  } deriving (Eq, Show, Generic)

instance Binary ManageOfferOp


data CreatePassiveOfferOp
  = CreatePassiveOfferOp
  { _selling :: Asset
  , _buying  :: Asset
  , _amount  :: Int64
  , _price   :: Price
  } deriving (Eq, Show, Generic)

instance Binary CreatePassiveOfferOp


newtype HomeDomain
  = HomeDomain
  { _homeDomain :: Text
  } deriving (Eq, Show)

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
  , _limit :: Maybe Int64
  } deriving (Eq, Show)

instance Binary ChangeTrustOp where
  put op = do
    op & put . _line
    op & put . fromMaybe 0 . _limit
  get = label "ChangeTrustOp" $ ChangeTrustOp
    <$> get
    <*> (getInt64be <&> mfilter (> 0) . Just)


data AllowTrustOp
  = AllowTrustOp
  { _trustor   :: PublicKey
  , _asset     :: Either AssetCode4 AssetCode12
  , _authorize :: Bool
  } deriving (Eq, Show)

instance Binary AllowTrustOp where
  put op = do
    op & put . _trustor
    either (put . (AssetTypeCreditAlphanum4,))
           (put . (AssetTypeCreditAlphanum12,))
           $ _asset (op :: AllowTrustOp)
    op & put . Padded . _authorize
  get = label "AllowTrustOp" $ do
    trustor <- get
    asset <- get >>= \case
      AssetTypeNative -> fail "Can't allow trust for a native asset"
      AssetTypeCreditAlphanum4  -> fmap Left  (get :: Get AssetCode4)
      AssetTypeCreditAlphanum12 -> fmap Right (get :: Get AssetCode12)
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
  , _maxTime :: Maybe Word64 -- 0 here means no maxTime
  } deriving (Eq, Show)

instance Binary TimeBounds where
  get = label "TimeBounds" $ do
    mn <- label "minTime" get
    mx <- label "maxTime" get
    pure $ TimeBounds mn $ if mx == 0 then Nothing else Just mx
  put (TimeBounds mn mx) = put mn >> put (fromMaybe 0 mx)


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

newtype SignatureHint
  = SignatureHint
  { _signatureHint :: Word32
  } deriving (Eq, Binary)

instance Show SignatureHint where
  show (SignatureHint w) = "SignatureHint "
    <> showByteString (BS.pack $ word32ToBytes w)


newtype Signature
  = Signature
  { _signature :: ED.Signature
  } deriving (Eq)

instance Show Signature where
  show (Signature ed) = "Signature " <> showByteString (BA.convert ed)

instance Binary Signature where
  put (Signature bs) = put (VarLen bs :: VarLen 64 ED.Signature)
  get = label "Signature" $ Signature <$> getVarLen (Proxy :: Proxy 64)


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
  get = label "EnvelopeType" getEnum
  put = putEnum


newtype Hash
  = Hash
  { _hash :: ByteString
  } deriving (Eq)

instance Show Hash where
  show (Hash bs) = "Hash {_hash = " <> showByteString bs <> "}"

instance Binary Hash where
  put (Hash bs) = put (FixLen bs :: FixLen 32 ByteString)
  get = label "Hash" $ Hash <$> getFixLen (Proxy :: Proxy 32)

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
