module Stellar.StrKey where

import qualified Crypto.Error              as CE
import qualified Crypto.PubKey.Ed25519     as ED
import qualified Data.Base32String.Default as B32
import qualified Data.ByteString           as BS
import           Data.Crc16                (crc16xmodem)
import           Protolude


data PublicKeyError
  = InvalidInputLength
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
  getPublicKeyBytes text = do
    let bytes = BS.unpack $ B32.toBytes $ B32.fromText text
    unless (length bytes == 35) $ throwError InvalidInputLength
    let (payload, checksum) = splitAt (length bytes - 2) bytes
    unless (toWord16le checksum == crc16xmodem payload)
      $ throwError InvalidChecksum
    let (version, key) = uncons payload & fromMaybe (0, [])
    unless (version == 0x30) $ throwError $ InvalidVersion version
    pure $ BS.pack key

  decodePublicKey :: ByteString -> Either PublicKeyError ED.PublicKey
  decodePublicKey = CE.onCryptoFailure (throwError . CryptoError) pure
    . ED.publicKey

  toWord16le :: [Word8] -> Word16
  toWord16le [h, l] = fromIntegral h .|. fromIntegral l `shiftL` 8
  toWord16le _      = 0

secretKey :: ByteString -> Either () ED.SecretKey
secretKey = notImplemented
--  "SBMX26OK2YUGXZ2XOEOLO5NBTKFSZ7T4HXZ4UB3DXIN43J7DKTMA2566"
