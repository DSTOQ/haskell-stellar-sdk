{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Stellar.Core.Types.BinarySpec where

import           Data.Binary.Extended
import qualified Data.ByteString.Lazy as BL
import           Data.String          (String, fromString)
import           Hedgehog
import           Protolude
import           Stellar.Gens


newtype Checkable
  = Checkable (∀ a. (Binary a, Show a, Eq a) => Gen a -> Property)

data Genable
  = ∀ a . (Binary a, Show a, Eq a)
  => Genable (String, Gen a)

run :: IO Bool
run = checkParallel $ Group "Binary Properties" $
  mkProp <$> checks <*> gens

mkProp :: (String, Checkable) -> Genable -> (PropertyName, Property)
mkProp (checkName, Checkable f) (Genable (typeName, g)) =
   (fromString $ checkName <> " " <> typeName, f g)

checks :: [(String, Checkable)]
checks =
  [ ("Roundtrip",     roundtrip)
  , ("Multiple of 4", isMultiple4)
  ]

gens :: [Genable]
gens =
  [ Genable ("NonNegativeInt64",     genNonNegativeInt64)
  , Genable ("Stroop",               genStroop)
  , Genable ("XLM",                  genXLM)
  , Genable ("PublicKeyType",        genPublicKeyType)
  , Genable ("PublicKey",            genPublicKey)
  , Genable ("SignerKeyType",        genSignerKeyType)
  , Genable ("SignerKey",            genSignerKey)
  , Genable ("Threshold",            genThreshold)
  , Genable ("PreciseAssetType",     genPreciseAssetType)
  , Genable ("Network",              genNetwork)
  , Genable ("Asset",                genAsset)
  , Genable ("Price",                genPrice)
  , Genable ("Fee",                  genFee)
  , Genable ("SequenceNumber",       genSequenceNumber)
  , Genable ("TimeBounds",           genTimeBounds)
  , Genable ("Sha256",               genSha256)
  , Genable ("Memo",                 genMemo)
  , Genable ("Signer",               genSigner)
  , Genable ("PaymentOp",            genPaymentOp)
  , Genable ("PathPaymentOp",        genPathPaymentOp)
  , Genable ("OfferId",              genOfferId)
  , Genable ("ManageOfferOp",        genManageOfferOp)
  , Genable ("CreatePassiveOfferOp", genCreatePassiveOfferOp)
  , Genable ("HomeDomain",           genHomeDomain)
  , Genable ("SetOptionsOp",         genSetOptionsOp)
  , Genable ("ChangeTrustOp",        genChangeTrustOp)
  , Genable ("AllowTrustOp",         genAllowTrustOp)
  , Genable ("DataValue",            genDataValue)
  , Genable ("ManageDataOp",         genManageDataOp)
  , Genable ("OperationType",        genOperationType)
  , Genable ("OperationBody",        genOperationBody)
  , Genable ("Operation",            genOperation)
  , Genable ("Transaction",          genTransaction)
  , Genable ("Signature",            genSignature)
  , Genable ("SignatureHint",        genSignatureHint)
  , Genable ("DecoratedSignature",   genDecoratedSignature)
  , Genable ("TransactionEnvelope",  genTransactionEnvelope)
  ]

roundtrip :: Checkable
roundtrip = Checkable $ \gen -> property $ do
  v <- forAll gen
  let encoded = encode v
  annotateShow $ BL.unpack encoded
  case decodeOrFail encoded of
    Left (_, _, err) -> do
      annotate err
      failure
    Right (unconsumed, _, decoded) -> do
      decoded === v
      BL.length unconsumed === 0

isMultiple4 :: Checkable
isMultiple4 = Checkable $ \gen -> property $ do
  v <- forAll gen
  BL.length (encode v) `rem` 4 === 0
