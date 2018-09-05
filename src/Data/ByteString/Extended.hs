module Data.ByteString.Extended
  ( module Data.ByteString
  , showByteString
  ) where

import           Data.ByteString
import           Data.ByteString.Base16 (encode)
import           Prelude                (String, show)
import           Protolude              hiding (show)

showByteString :: ByteString -> String
showByteString = show . unpack . encode
