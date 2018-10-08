module Stellar.Core.Types.Key.Parser where

import           Control.Arrow                  (left)
import           Crypto.Error                   (CryptoError, CryptoFailable,
                                                 onCryptoFailure)
import           Data.ByteArray.Encoding        (Base (Base32), convertFromBase)
import qualified Data.ByteString.Extended       as BS
import           Data.Crc16                     (crc16xmodem)
import           Data.Word.Extended             (word16FromBytes)
import           Prelude                        (String)
import           Protolude
import           Stellar.Core.Types.Key.Version (KeyVersion, keyVersionByte)


data Error
  = InvalidInputLength
  | InvalidBase32Encoding String
  | InvalidVersion Word8
  | InvalidChecksum
  | CryptoError CryptoError
  deriving (Eq, Show)

fromText
  :: KeyVersion
  -> (ByteString -> CryptoFailable k)
  -> Text -> Either Error k
fromText ver f =
  getKeyBytes ver >=> onCryptoFailure (throwError . CryptoError) pure . f

fromBase32 :: ByteString -> Either Error ByteString
fromBase32 = left InvalidBase32Encoding . convertFromBase Base32

getKeyBytes :: KeyVersion -> Text -> Either Error ByteString
getKeyBytes ver text = do
  bytes <- BS.unpack <$> fromBase32 (toS text)
  unless (length bytes == 35) $ throwError InvalidInputLength
  let (payload, checksum) = splitAt (length bytes - 2) bytes
  unless (word16FromBytes (reverse checksum) == crc16xmodem payload)
    $ throwError InvalidChecksum
  let (version, key) = uncons payload & fromMaybe (0, [])
  unless (version == keyVersionByte ver) $ throwError $ InvalidVersion version
  pure $ BS.pack key
