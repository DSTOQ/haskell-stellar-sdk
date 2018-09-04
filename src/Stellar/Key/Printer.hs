module Stellar.Key.Printer
  ( printPublicKey
  , printSecretKey
  ) where

import qualified Crypto.PubKey.Ed25519     as ED
import qualified Data.Base32String.Default as B32
import qualified Data.ByteArray            as BA
import qualified Data.ByteString           as BS
import           Data.Crc16                (crc16xmodem)
import           Data.Word.Extended        (word16ToBytes)
import           Protolude
import           Stellar.Key.Version


printPublicKey :: ED.PublicKey -> Text
printPublicKey = toText AccountId . BA.convert

printSecretKey :: ED.SecretKey -> Text
printSecretKey = toText Seed . BA.convert

toText :: KeyVersion -> ByteString -> Text
toText version
  = B32.toText
  . B32.fromBytes
  . appendChecksum
  . BS.cons (keyVersionByte version)
  . BA.convert

  where

  appendChecksum :: ByteString -> ByteString
  appendChecksum bs
    = BS.append bs
    . BS.pack
    . reverse
    . word16ToBytes
    . crc16xmodem
    . BS.unpack
    $ bs
