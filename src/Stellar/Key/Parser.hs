module Stellar.Key.Parser
  ( parsePublicKey
  , parseSecretKey
  , Error (..)
  ) where

import           Crypto.Error              (CryptoError, CryptoFailable,
                                            onCryptoFailure)
import qualified Crypto.PubKey.Ed25519     as ED
import qualified Data.Base32String.Default as B32
import qualified Data.ByteString           as BS
import           Data.Crc16                (crc16xmodem)
import           Data.Word.Extended        (word16FromOctets)
import           Protolude


-- Example: GAMRCWH3Q2WHEGTNNNUQEF2VP46XYULGAH6RYJTBO44YZX5CY7YE7QRD
parsePublicKey :: Text -> Either Error ED.PublicKey
parsePublicKey = fromText 0x30 ED.publicKey

-- Example: SBMX26OK2YUGXZ2XOEOLO5NBTKFSZ7T4HXZ4UB3DXIN43J7DKTMA2566
parseSecretKey :: Text -> Either Error ED.SecretKey
parseSecretKey = fromText 0x90 ED.secretKey

type Ver = Word8

data Error
  = InvalidInputLength
  | InvalidVersion Word8
  | InvalidChecksum
  | CryptoError CryptoError
  deriving (Eq, Show)

fromText :: Ver -> (ByteString -> CryptoFailable k) -> Text -> Either Error k
fromText ver f = getKeyBytes ver
               >=> onCryptoFailure (throwError . CryptoError) pure . f

getKeyBytes :: Ver -> Text -> Either Error ByteString
getKeyBytes ver text = do
  let bytes = BS.unpack $ B32.toBytes $ B32.fromText text
  unless (length bytes == 35) $ throwError InvalidInputLength
  let (payload, checksum) = splitAt (length bytes - 2) bytes
  unless (word16FromOctets (reverse checksum) == crc16xmodem payload)
    $ throwError InvalidChecksum
  let (version, key) = uncons payload & fromMaybe (0, [])
  unless (version == ver) $ throwError $ InvalidVersion version
  pure $ BS.pack key
