{-# LANGUAGE TemplateHaskell #-}
module Stellar.Lens where

import Control.Lens.TH
import Stellar.Types

makeFieldsNoPrefix ''TransactionEnvelope
makeFieldsNoPrefix ''DecoratedSignature
makeFieldsNoPrefix ''Signature
makeFieldsNoPrefix ''SignatureHint
makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''TimeBounds
makeFieldsNoPrefix ''Operation
makeFieldsNoPrefix ''OperationBody
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
makeFieldsNoPrefix ''Asset
makeFieldsNoPrefix ''AssetCode
makeFieldsNoPrefix ''NonNativeAsset
makeFieldsNoPrefix ''Threshold
makeFieldsNoPrefix ''SignerKey
makeFieldsNoPrefix ''PublicKey
makeFieldsNoPrefix ''SecretKey
makeFieldsNoPrefix ''KeyPair
