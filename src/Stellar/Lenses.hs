{-# LANGUAGE TemplateHaskell #-}

module Stellar.Lenses where


import           Control.Lens.TH
import           Stellar.Types

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
makeFieldsNoPrefix ''Hash
makeFieldsNoPrefix ''SequenceNumber
makeFieldsNoPrefix ''Fee
makeFieldsNoPrefix ''Price
makeFieldsNoPrefix ''Asset
makeFieldsNoPrefix ''AssetCode12
makeFieldsNoPrefix ''AssetCode4
makeFieldsNoPrefix ''Threshold
makeFieldsNoPrefix ''SignerKey
makeFieldsNoPrefix ''PublicKey
makeFieldsNoPrefix ''Asset
