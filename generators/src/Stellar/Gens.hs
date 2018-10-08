module Stellar.Gens
  ( genAccountId
  , genAccount
  , genBalance
  , genBalanceTrustline
  , genNonNegativeInt64
  , genStroop
  , genXLM
  , genEdSecretKey
  , genEdPublicKey
  , genSecretKey
  , SecretKeyText (..)
  , genKeyPair
  , genPublicKeyType
  , genPublicKey
  , PublicKeyText (..)
  , genSignerKeyType
  , genSignerKey
  , genThreshold
  , genAssetCode
  , genAssetType
  , genPreciseAssetType
  , genNonNativeAsset
  , genAsset
  , genPrice
  , genFee
  , genSequenceNumber
  , genTimeBounds
  , genSha256
  , genMemo
  , genSigner
  , genCreateAccountOp
  , genPaymentOp
  , genPathPaymentOp
  , genOfferId
  , genManageOfferOp
  , genCreatePassiveOfferOp
  , genHomeDomain
  , genSetOptionsOp
  , genChangeTrustOp
  , genAllowTrustOp
  , genDataValue
  , genManageDataOp
  , genOperationType
  , genOperationBody
  , genOperation
  , genTransaction
  , genTransactionId
  , genTransactionDetails
  , genCursor
  , genLedger
  , genSignatureHint
  , genSignature
  , genDecoratedSignature
  , genTransactionEnvelope
  , genNetwork
  , genSecretKeyText
  , genPublicKeyText
  , genKeyParser
  , KeyParser (..)
  ) where

import           Control.Newtype             (pack)
import           Crypto.Error                (throwCryptoError)
import qualified Crypto.PubKey.Ed25519       as ED
import qualified Data.StaticText             as S
import           Hedgehog
import qualified Hedgehog.Gen.Extended       as Gen
import qualified Hedgehog.Range              as Range
import           Protolude
import           Refined
import           Stellar.Client
import           Stellar.Core.Types.Internal
import           Text.Show                   (show)

genAccountId :: Gen AccountId
genAccountId = unsafeAccountId <$> Gen.element
  [ "GACGGY434KV54ZGHN7DYMYARP5MAEUHOOTHKFMGBBLGKXMKZ3WULZEIG"
  , "GAQKLAZ6N2DNT6KJHPV7IVYPSVEKHFGTO3OUS4U5BKTWKTEGHBY52WDX"
  , "GDSD4HQBHBZ2ONM7KDGYNOOGPSWNUKTHYNZQ23DLP7SOJVVOBUMFLBN5"
  , "GBLOTNOCLZRTYZ6QPDBYHTMV6TNPRSO3PW3D6MYVRXWGQP6C3XTJTXN4"
  , "GBCJ6VH3USUSSNNTXKAB4JMZVVWTRT4VJAR7UBQUII2ZWWXLBP7SUXR7"
  , "GDWVNRC2BF2RHD6XGKQLEEAGJY7AMDLCDN3GEFRAW24HGA5ZO5NULYQ5"
  , "GCUCMXJGLWUB3CYLXXOL653AKZXAR3YQHKLTXCHFSEMP5INBAYD7MOQE"
  , "GCTGWMFBY4SPHBM5K5KDIS6Q7F5EHW47SR6JPVYG2XBN7OWUYLS2KOGO"
  , "GBHIVIIKB7JSFXOSA7OTWWSO542WJRAOVGZAODFU3IYDUWH4MVCWYS3W"
  , "GBGUXTGFCDPTJRUI3YDT7U6FNZUM7XFJZGVC5C5HLXX6LLH3LRJG4VM5"
  , "GD4XP2UWXPGRMZHNQM22WV3GI4VMSQZK5Y2SH2HAYTULZ6LWYZWHVA5I"
  , "GBPUG3KJLDOGW6MDTJ6VTSWJAL4GUZ53DMCE5COSANX34OS67TR4JAEK"
  ]

genThresholds :: Gen Thresholds
genThresholds = Thresholds
  <$> genThreshold
  <*> genThreshold
  <*> genThreshold

genLiabilities :: Gen Liabilities
genLiabilities = Liabilities <$> genNonNegativeInt64 <*> genNonNegativeInt64

genTrustline :: Gen Trustline
genTrustline = Trustline <$> genNonNativeAsset <*> genNonNegativeInt64

genBalance :: Gen Balance
genBalance = Balance
  <$> genLiabilities
  <*> Gen.choice
      [ Left <$> genStroop
      , Right <$> ((,) <$> genTrustline <*> genNonNegativeInt64)
      ]

genBalanceTrustline :: Gen Balance
genBalanceTrustline = Balance
  <$> genLiabilities
  <*> fmap Right genNonNativeBalance

genNonNativeBalance :: Gen (Trustline, NonNegativeInt64)
genNonNativeBalance = (,) <$> genTrustline <*> genNonNegativeInt64

genAccountFlags :: Gen AccountFlags
genAccountFlags = AccountFlags <$> Gen.bool <*> Gen.bool

genDataValues :: Gen (Map Text DataValue)
genDataValues = Gen.map (Range.linear 0 10) genKV
  where genKV = (,) <$> genKey <*> genDataValue
        genKey = Gen.text (Range.exponential 1 1000) Gen.ascii

genAccount :: Gen Account
genAccount = Account
  <$> genAccountId
  <*> genPublicKey
  <*> genSequenceNumber
  <*> Gen.expWord32
  <*> genThresholds
  <*> Gen.list (Range.linear 0 10) genBalance
  <*> genAccountFlags
  <*> Gen.list (Range.linear 0 5) genSigner
  <*> genDataValues

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

genNonNativeAsset :: Gen NonNativeAsset
genNonNativeAsset = NonNativeAsset <$> genAssetCode <*> genPublicKey

genAsset :: Gen Asset
genAsset = Gen.choice
  [pure AssetNative, AssetCreditAlphanum <$> genNonNativeAsset]

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

genTransactionId :: Gen TransactionId
genTransactionId = TransactionId <$> genSha256

genTransactionDetails :: Gen TransactionDetails
genTransactionDetails = TransactionDetails
  <$> genTransactionId
  <*> genLedger

genCursor :: Gen Cursor
genCursor = pack <$> Gen.text (Range.linear 1 10) Gen.alphaNum

genLedger :: Gen Ledger
genLedger = Ledger <$> Gen.expWord64

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


newtype SecretKeyText = SecretKeyText Text deriving (Eq, Show)

genSecretKeyText :: Gen SecretKeyText
genSecretKeyText = SecretKeyText <$> Gen.element
 [ "SBM2P34FJWQ6L2OPB2PYYWMVELEZP3GCM62QGZ2MYDEUMDUO7LPAJVTV"
 , "SDMGML6Z7WYMNY4SEAF4UTMMYD2JJUD22Q5TSH6KCLWKNTBS5P3JSZWL"
 , "SANOKDUGKISMIGK3U33UYT2YQET4SJJZPMASK2LM44W5EINRK43IMJXO"
 , "SCQ224PKWZIPKRF6HBHG3TZEIIQKBIKXW6RVFUKIHHPK4MZFY4KBONZ2"
 , "SDF5H3CUGX7II2IZ5XMOQDFCMCWZWY7QUQT4LP4FXVCCGY5ZL737E7KO"
 , "SDOXGP6QETPUPS4F7NGDUPTXSQQ4FHPUZI7IF32YOK7FCJ4T2E7SP7JM"
 , "SBI3JQVGDSWYBTPWXTCYYURUMAZ7UOYICDM2S72EVDKRQWPRPKH3GQGM"
 , "SAY666T7IQPZIMZHT2QWNUGXZMR25CSDS6VU3SNLL7DB4GNX2JG2PKNL"
 , "SDDYVMVH5OEPAVSXFDVVBTKMCDCYNJYUQKN67VEFIIPBLS4M2PVUTCPO"
 , "SC73IPZYRC6PGS7WLHRG4X46A4Y4JZ6XBSIYPSMOYW5A4DXDVHEBMOT4"
 , "SA6OFJGVZVXRFACIJ5Y6NVLWBELGKIF4PZEYTPOJLGDYSK2K6MDV67FQ"
 ]

newtype PublicKeyText = PublicKeyText Text deriving (Eq, Show)

genPublicKeyText :: Gen PublicKeyText
genPublicKeyText = PublicKeyText <$> Gen.element
  [ "GA4IBZJDA2K3JWC3L6XOKJLHCKES63HXVI2XCDN4SZ7FODVNL2QJY6GF"
  , "GCNSGHUCG5VMGLT5RIYYZSO7VQULQKAJ62QA33DBC5PPBSO57LFWVV6P"
  , "GAOMLS5MFQNPNUP6AALYAAH2OWTATB5ICS6XWTZ364UVGSDQVMFBKRMH"
  , "GBB7JKBP5ZG7UUHAOYDOHQMIVDRKNMXTCDU3WUDVRV77NZJBEJNL4F2H"
  , "GCZ6KEBDPV6AJ7E4VISM5VKAK6ZR3CMIKRBO2TMZHT37NHLBL34OVJOZ"
  , "GAREELUB43IRHWEASCFBLKHURCGMHE5IF6XSE7EXDLACYHGRHM43RFOX"
  , "GA77B6GK5K3FH2YJ6I5VJ7VPFZKPBQUX2IIC2MJYAERQTGJI4VOPKRYJ"
  , "GCIM3EHKJAUURMEYETQ2ZG4DSP2PAJDDVLYDSCROC2ELXGYJBM6PF4EH"
  , "GBROS6JISSGRJRMWMC2GVD2YTFFZ7L24VCCE5D76P6LWO7D5GDD5ABJG"
  , "GCKTBRR7KIR34J2U6MIL3O2UCQT6LW7U25TFX7ZVBHH2MLCSDCRF64PD"
  , "GDV5U356L5MZF4J6NM5UWWXCMEJRZU35YWLWY5E7PMQKBF64VW36KCDE"
  ]


data KeyParser = forall o. KeyParser (Text -> Either Error o)
instance Show KeyParser where show _ = "<key parser>"

genKeyParser :: Gen KeyParser
genKeyParser = Gen.element [ KeyParser parseSecretKey
                           , KeyParser parsePublicKey
                           ]
