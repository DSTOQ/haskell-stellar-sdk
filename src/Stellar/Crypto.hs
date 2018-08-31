module Stellar.Crypto where


import           Control.Lens
import qualified Crypto.PubKey.Ed25519 as ED
import           Data.Binary           (encode)
import qualified Data.ByteString.Lazy  as LBS
import           Protolude
import qualified Stellar.Lenses        as L
import           Stellar.Types

signTransaction :: KeyPair -> TransactionEnvelope -> TransactionEnvelope
signTransaction keys envelope =
  let signature = DecoratedSignature (keys ^. L.hint)
       $ Signature $ ED.sign (keys ^. L.secretKey) (keys ^. L.publicKey)
       $ LBS.toStrict . encode $ envelope ^. L.transaction
  in envelope & L.signatures %~ cons signature
