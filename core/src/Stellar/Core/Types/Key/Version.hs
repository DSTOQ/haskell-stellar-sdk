module Stellar.Core.Types.Key.Version
  ( KeyVersion(..)
  , keyVersionByte
  , keyByteVersion
  ) where

import Data.Binary.Extended
import Data.Binary.Get      (label)
import Protolude

data KeyVersion
  = AccountId
  | Seed
  | PreAuthTx
  | Sha256Hash
  deriving (Eq, Show, Enum, Bounded)

instance Binary KeyVersion where
  get = label "Version" getEnum
  put = putEnum

keyVersionByte :: KeyVersion -> Word8
keyVersionByte v = case v of
  AccountId  -> 0x30 -- G
  Seed       -> 0x90 -- S
  PreAuthTx  -> 0x98 -- T
  Sha256Hash -> 0xB8 -- X

keyByteVersion :: Word8 -> Maybe KeyVersion
keyByteVersion b = case b of
  0x30 -> pure AccountId
  0x90 -> pure Seed
  0x98 -> pure PreAuthTx
  0xB8 -> pure Sha256Hash
  _    -> Nothing
