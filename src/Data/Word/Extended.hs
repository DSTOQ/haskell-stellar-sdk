module Data.Word.Extended
  ( module Data.Word
  , word32FromBytes
  , word32ToBytes
  , word16FromBytes
  , word16ToBytes
  ) where

import           Data.Bits
import           Data.Word
import           Protolude

word32ToBytes :: Word32 -> [Word8]
word32ToBytes w =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

word32FromBytes :: [Word8] -> Word32
word32FromBytes = foldl' accum 0
  where
  accum :: Word32 -> Word8 -> Word32
  accum a o = a `shiftL` 8 .|. fromIntegral o

word16ToBytes :: Word16 -> [Word8]
word16ToBytes w =
    [ fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

word16FromBytes :: [Word8] -> Word16
word16FromBytes [h, l] = fromIntegral h `shiftL` 8 .|. fromIntegral l
word16FromBytes _      = 0
