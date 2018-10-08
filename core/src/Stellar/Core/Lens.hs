{-# LANGUAGE TemplateHaskell #-}
module Stellar.Core.Lens where

import Control.Lens
import Stellar.Core.Types

makeFieldsNoPrefix ''TransactionEnvelope
makeFieldsNoPrefix ''DecoratedSignature
makeFieldsNoPrefix ''Signature
makeFieldsNoPrefix ''SignatureHint
makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''TimeBounds
makeFieldsNoPrefix ''Operation
makeFieldsNoPrefix ''ManageDataOp
makeFieldsNoPrefix ''AllowTrustOp
makeFieldsNoPrefix ''ChangeTrustOp
makeFieldsNoPrefix ''SetOptionsOp
makeFieldsNoPrefix ''HomeDomain
makeFieldsNoPrefix ''ManageOfferOp
makeFieldsNoPrefix ''CreatePassiveOfferOp
makeFieldsNoPrefix ''OfferId
makeFieldsNoPrefix ''PathPaymentOp
makeFieldsNoPrefix ''PaymentOp
makeFieldsNoPrefix ''CreateAccountOp
makeFieldsNoPrefix ''Signer
makeFieldsNoPrefix ''Memo
makeFieldsNoPrefix ''SequenceNumber
makeFieldsNoPrefix ''Fee
makeFieldsNoPrefix ''Price
makeFieldsNoPrefix ''AssetCode
makeFieldsNoPrefix ''NonNativeAsset
makeFieldsNoPrefix ''Threshold
makeFieldsNoPrefix ''SignerKey
makeFieldsNoPrefix ''PublicKey
makeFieldsNoPrefix ''SecretKey
makeFieldsNoPrefix ''KeyPair

makePrisms ''Asset
makePrisms ''Memo
makePrisms ''MemoType
makePrisms ''OperationBody
makePrisms ''OperationType
