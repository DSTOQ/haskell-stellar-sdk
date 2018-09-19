module Data.ByteString.Extended
  ( module Data.ByteString
  , printByteStringBase16
  , printByteStringBase64
  ) where

import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString
import Data.Text.Encoding      (decodeUtf8)
import Protolude


printByteStringBase16 :: ByteString -> Text
printByteStringBase16 = decodeUtf8 . convertToBase Base16

printByteStringBase64 :: ByteString -> Text
printByteStringBase64 = decodeUtf8 . convertToBase Base64
