module Stellar.Gens where

import           Control.Newtype        (pack)
import           Crypto.Error           (throwCryptoError)
import qualified Crypto.PubKey.Ed25519  as ED
import qualified Data.StaticText        as S
import           Hedgehog
import qualified Hedgehog.Gen.Extended  as Gen
import qualified Hedgehog.Range         as Range
import           Protolude
import           Refined
import           Stellar.Types
import           Stellar.Types.Internal

genNonNegativeInt64 :: Gen NonNegativeInt64
genNonNegativeInt64 = do
  int64 <- Gen.int64 (Range.exponential 0 maxBound)
  pure $ pack $ unsafeRefine int64

genStroop :: Gen Stroop
genStroop = pack <$> Gen.expInt64

genXLM :: Gen XLM
genXLM = XLM <$> genStroop

genEdSecretKey :: Gen ED.SecretKey
genEdSecretKey = throwCryptoError . ED.secretKey
  <$> Gen.bytes (Range.singleton 32)

genSecretKey :: Gen SecretKey
genSecretKey = pack <$> genEdSecretKey

genEdPublicKey :: Gen ED.PublicKey
genEdPublicKey = throwCryptoError . ED.publicKey
  <$> Gen.bytes (Range.singleton 32)

genKeyPair :: Gen KeyPair
genKeyPair = keyPair' <$> genSecretKey

genPublicKeyType :: Gen PublicKeyType
genPublicKeyType = Gen.enumBounded

genPublicKey :: Gen PublicKey
genPublicKey = pack <$> genEdPublicKey

genSignerKeyType :: Gen SignerKeyType
genSignerKeyType = Gen.enumBounded

genSignerKey :: Gen SignerKey
genSignerKey = Gen.choice
  [ SignerKeyEd25519 <$> genPublicKey
  , SignerKeyPreAuthTx <$> genSha256
  , SignerKeyHashX <$> genSha256
  ]

genThreshold :: Gen Threshold
genThreshold = pack <$> Gen.expWord32

genAssetCode :: Gen AssetCode
genAssetCode = unsafeAssetCode . toS
  <$> Gen.text (Range.linear 1 12) Gen.alphaNum

genAssetType :: Gen AssetType
genAssetType = Gen.enumBounded

genPreciseAssetType :: Gen PreciseAssetType
genPreciseAssetType = Gen.enumBounded

genAsset :: Gen Asset
genAsset = Gen.choice
  [pure AssetNative, AssetCreditAlphanum <$> genAssetCode <*> genPublicKey]

genPrice :: Gen Price
genPrice = Price <$> Gen.expInt32 <*> Gen.expInt32

genFee :: Gen Fee
genFee = pack <$> Gen.expWord32

genSequenceNumber :: Gen SequenceNumber
genSequenceNumber = pack <$> genNonNegativeInt64

genTimeBounds :: Gen TimeBounds
genTimeBounds = TimeBounds
  <$> Gen.expWord64
  <*> Gen.word64 Range.exponentialBounded

genSha256 :: Gen Sha256
genSha256 = pack . S.unsafeCreate <$> Gen.bytes (Range.singleton 32)

genMemo :: Gen Memo
genMemo = Gen.choice
  [ pure MemoNone
  , MemoText <$> Gen.bytes (Range.linear 0 27)
  , MemoId <$> Gen.expWord64
  , MemoHash <$> genSha256
  , MemoReturn <$> genSha256
  ]

genSigner :: Gen Signer
genSigner = Signer
  <$> genSignerKey
  <*> Gen.expWord32

genCreateAccountOp :: Gen CreateAccountOp
genCreateAccountOp = CreateAccountOp
  <$> genPublicKey
  <*> genStroop

genPaymentOp :: Gen PaymentOp
genPaymentOp = PaymentOp
  <$> genPublicKey
  <*> genAsset
  <*> genNonNegativeInt64

genPathPaymentOp :: Gen PathPaymentOp
genPathPaymentOp = PathPaymentOp
  <$> genAsset
  <*> genNonNegativeInt64
  <*> genPublicKey
  <*> genAsset
  <*> genNonNegativeInt64
  <*> Gen.list (Range.linear 0 5) genAsset

genOfferId :: Gen OfferId
genOfferId = pack <$> Gen.expWord64

genManageOfferOp :: Gen ManageOfferOp
genManageOfferOp = ManageOfferOp
  <$> genAsset
  <*> genAsset
  <*> genNonNegativeInt64
  <*> genPrice
  <*> genOfferId

genCreatePassiveOfferOp :: Gen CreatePassiveOfferOp
genCreatePassiveOfferOp = CreatePassiveOfferOp
  <$> genAsset
  <*> genAsset
  <*> genNonNegativeInt64
  <*> genPrice

genHomeDomain :: Gen HomeDomain
genHomeDomain = pack <$> Gen.text (Range.linear 1 32) Gen.ascii

genSetOptionsOp :: Gen SetOptionsOp
genSetOptionsOp = SetOptionsOp
  <$> Gen.maybe genPublicKey
  <*> Gen.maybe Gen.expWord32
  <*> Gen.maybe Gen.expWord32
  <*> Gen.maybe Gen.expWord32
  <*> Gen.maybe genThreshold
  <*> Gen.maybe genThreshold
  <*> Gen.maybe genThreshold
  <*> Gen.maybe genHomeDomain
  <*> Gen.maybe genSigner

genChangeTrustOp :: Gen ChangeTrustOp
genChangeTrustOp = ChangeTrustOp <$> genAsset <*> genNonNegativeInt64

genAllowTrustOp :: Gen AllowTrustOp
genAllowTrustOp = AllowTrustOp
  <$> genPublicKey
  <*> genAssetCode
  <*> Gen.bool

genDataValue :: Gen DataValue
genDataValue = DataValue . VarLen <$> Gen.bytes (Range.linear 0 64)

genManageDataOp :: Gen ManageDataOp
genManageDataOp = ManageDataOp
  <$> (VarLen <$> Gen.text (Range.linear 1 10) Gen.ascii)
  <*> Gen.maybe genDataValue

genOperationType :: Gen OperationType
genOperationType = Gen.element
  [ OperationTypeCreateAccount
  , OperationTypePayment
  , OperationTypePathPayment
  , OperationTypeManageOffer
  , OperationTypeCreatePassiveOffer
  , OperationTypeSetOptions
  , OperationTypeChangeTrust
  , OperationTypeAllowTrust
  , OperationTypeAccountMerge
  , OperationTypeInflation
  , OperationTypeManageData
  , OperationTypeBumpSequence
  ]

genOperationBody :: Gen OperationBody
genOperationBody = Gen.choice
  [ CreateAccount <$> genCreateAccountOp
  , Payment <$> genPaymentOp
  , PathPayment <$> genPathPaymentOp
  , ManageOffer <$> genManageOfferOp
  , CreatePassiveOffer <$> genCreatePassiveOfferOp
  , SetOptions <$> genSetOptionsOp
  , ChangeTrust <$> genChangeTrustOp
  , AllowTrust <$> genAllowTrustOp
  , AccountMerge <$> genPublicKey
  , pure Inflation
  , ManageData <$> genManageDataOp
  , BumpSequence <$> genSequenceNumber
  ]

genOperation :: Gen Operation
genOperation = Operation
  <$> Gen.maybe genPublicKey
  <*> genOperationBody

genTransaction :: Gen Transaction
genTransaction = Transaction
  <$> genPublicKey
  <*> genFee
  <*> genSequenceNumber
  <*> Gen.maybe genTimeBounds
  <*> genMemo
  <*> Gen.list (Range.exponential 1 10) genOperation

genSignatureHint :: Gen SignatureHint
genSignatureHint = pack <$> Gen.expWord32

genSignature :: Gen Signature
genSignature = pack . throwCryptoError . ED.signature
  <$> Gen.bytes (Range.singleton 64)

genDecoratedSignature :: Gen DecoratedSignature
genDecoratedSignature = DecoratedSignature
  <$> genSignatureHint
  <*> genSignature

genTransactionEnvelope :: Gen TransactionEnvelope
genTransactionEnvelope = TransactionEnvelope
  <$> genTransaction
  <*> Gen.list (Range.linear 0 3) genDecoratedSignature

genNetwork :: Gen Network
genNetwork = Gen.element [Public, Testnet]
