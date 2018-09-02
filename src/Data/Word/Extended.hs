module Data.Word.Extended
  ( module Data.Word
  , word32FromOctets
  , word32ToOctets
  , word16FromOctets
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
  accum a o = a `shiftL` 8 .|. fromIntegral o

word16FromOctets :: [Word8] -> Word16
word16FromOctets [h, l] = fromIntegral h `shiftL` 8 .|. fromIntegral l
word16FromOctets _      = 0
