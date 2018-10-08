module Stellar.Core.Types.Key.Printer where

import qualified Data.ByteArray                 as BA
import           Data.ByteArray.Encoding        (Base (Base32), convertToBase)
import qualified Data.ByteString                as BS
import           Data.Crc16                     (crc16xmodem)
import           Data.Word.Extended             (word16ToBytes)
import           Protolude
import           Stellar.Core.Types.Key.Version


toText :: KeyVersion -> ByteString -> Text
toText version
  = (toS :: ByteString -> Text)
  . convertToBase Base32
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
