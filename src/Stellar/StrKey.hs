module Stellar.StrKey where

import qualified Crypto.Error              as CE
import qualified Crypto.PubKey.Ed25519     as ED
import qualified Data.Base32String.Default as B32
import qualified Data.ByteString           as BS
import           Data.List                 (tail)
import           Protolude


data PublicKeyError
  = EmptyInput
  | InvalidVersion Word8
  | InvalidChecksum
  | CryptoError CE.CryptoError
  deriving (Eq, Show)


-- Example: GAMRCWH3Q2WHEGTNNNUQEF2VP46XYULGAH6RYJTBO44YZX5CY7YE7QRD
publicKey :: Text -> Either PublicKeyError ED.PublicKey
publicKey = getPublicKeyBytes >=> decodePublicKey

  where

  -- https://github.com/stellar/java-stellar-sdk/blob/master
  -- /src/main/java/org/stellar/sdk/StrKey.java#L97
  getPublicKeyBytes :: Text -> Either PublicKeyError ByteString
  getPublicKeyBytes t = do
    (version, bytes) <- uncons (BS.unpack . B32.toBytes . B32.fromText $ t)
                        & maybe (throwError EmptyInput) pure
    let (payload, checksum) = splitAt (length bytes - 2) bytes
        payloadChecksum pl = notImplemented
    unless (version == 0x30) (throwError $ InvalidVersion version)
    unless (checksum == payloadChecksum payload) (throwError InvalidChecksum)
    pure $ BS.pack $ tail payload

  decodePublicKey :: ByteString -> Either PublicKeyError ED.PublicKey
  decodePublicKey = CE.onCryptoFailure (throwError . CryptoError) pure
    . ED.publicKey


secretKey :: ByteString -> Either () ED.SecretKey
secretKey = notImplemented
--  "SBMX26OK2YUGXZ2XOEOLO5NBTKFSZ7T4HXZ4UB3DXIN43J7DKTMA2566"
