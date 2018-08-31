module Data.Word.Extended
  ( module Data.Word
  , word32FromOctets
  , word32ToOctets
  ) where

import           Data.Bits
import           Data.Word
import           Protolude

word32ToOctets :: Word32 -> [Word8]
word32ToOctets w =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

word32FromOctets :: [Word8] -> Word32
word32FromOctets = foldl' accum 0
  where
  accum :: Word32 -> Word8 -> Word32
  accum a o = (a `shiftL` 8) .|. fromIntegral o
