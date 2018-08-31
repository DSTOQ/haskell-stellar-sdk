module Stellar.Crypto where

import           Control.Lens
import qualified Crypto.Hash           as CH
import qualified Crypto.PubKey.Ed25519 as ED
import           Data.Binary.Extended
import           Data.Binary.Put       (runPut)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Lazy  as LBS
import           Protolude             hiding (put)
import qualified Stellar.Lenses        as L
import           Stellar.Types


signTransactionEnvelope
  :: Network
  -> KeyPair
  -> TransactionEnvelope
  -> TransactionEnvelope
signTransactionEnvelope network keys envelope =
  envelope & L.signatures %~ cons signature
  where signature = signTransaction network keys (envelope ^. L.transaction)

signTransaction :: Network -> KeyPair -> Transaction -> DecoratedSignature
signTransaction network keys tx = DecoratedSignature (keys ^. L.hint)
  $ Signature $ ED.sign (keys ^. L.secretKey) (keys ^. L.publicKey)
              $ txSignaturePayload network tx

txSignaturePayload :: Network -> Transaction -> ByteString
txSignaturePayload net tx = sha256 . LBS.toStrict $ runPut $
  put (networkHash net) >> put EnvelopeTypeTx >> put tx

networkHash :: Network -> Hash
networkHash net = Hash $ case net of
  Public  -> sha256 "Public Global Stellar Network ; September 2015"
  Testnet -> sha256 "Test SDF Network ; September 2015"

sha256 :: ByteString -> ByteString
sha256 = BA.convert . sha256digest
  where
  sha256digest :: ByteString -> CH.Digest CH.SHA256
  sha256digest = CH.hash
