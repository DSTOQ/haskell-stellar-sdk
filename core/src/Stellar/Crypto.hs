module Stellar.Crypto where

import           Control.Lens
import qualified Crypto.Hash           as CH
import qualified Crypto.PubKey.Ed25519 as ED
import           Data.Binary.Extended
import           Data.Binary.Put       (runPut)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.StaticText       as S
import           Protolude             hiding (put)
import qualified Stellar.Core.Lens     as L
import           Stellar.Core.Types


signTransactionEnvelope
  :: Network
  -> KeyPair
  -> TransactionEnvelope
  -> TransactionEnvelope
signTransactionEnvelope network keys envelope =
  envelope & L.signatures %~ cons signature
  where signature = signTransaction network keys (envelope ^. L.transaction)

signTransactionAsEnvelope
  :: Network
  -> KeyPair
  -> Transaction
  -> TransactionEnvelope
signTransactionAsEnvelope network keys tx =
  TransactionEnvelope tx [signTransaction network keys tx]

signTransaction :: Network -> KeyPair -> Transaction -> DecoratedSignature
signTransaction network keys tx =
  let secret = keys ^. (L.secretKey . L.secretKeyEd25519)
      public = keys ^. (L.publicKey . L.publicKeyEd25519)
      payload = txSignatureBase network tx
      signature = Signature $ ED.sign secret public payload
  in DecoratedSignature (keys ^. L.hint) signature


txSignatureBase :: Network -> Transaction -> ByteString
txSignatureBase net tx = sha256 . LBS.toStrict $ bytes
  where
  bytes = runPut $ do
    put (networkHash net)
    put EnvelopeTypeTx
    put tx

networkHash :: Network -> Sha256
networkHash net = Sha256 $ case net of
  Public  -> S.unsafeCreate $ sha256 "Public Global Stellar Network ; September 2015"
  Testnet -> S.unsafeCreate $ sha256 "Test SDF Network ; September 2015"

sha256 :: ByteString -> ByteString
sha256 = BA.convert . sha256digest
  where
  sha256digest :: ByteString -> CH.Digest CH.SHA256
  sha256digest = CH.hash
