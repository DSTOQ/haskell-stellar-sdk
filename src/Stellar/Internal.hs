{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stellar.Internal where

import           Control.Monad        (fail)
import           Data.Binary.Extended
import           Data.Binary.Get      (getByteString, getWord32be, skip)
import           Data.ByteString      as BS
import           GHC.TypeLits
import           Protolude            hiding (get, put)


newtype VarLen (n :: Nat) a
  = VarLen
  { unVarLen :: a
  } deriving (Eq, Show)

instance KnownNat n => Binary (VarLen n ByteString) where
  put (VarLen bs) = putPaddedByteString bs
  get = do
    len <- getWord32be <&> fromIntegral
    let cap = fromInteger $ natVal (Proxy :: Proxy n)
    if len > cap
      then fail $ "Max length (" <> show cap <> ") exceeded (" <> show len <> ")"
      else do
        bs <- getByteString len
        skip $ padding 4 len
        pure $ VarLen bs

instance KnownNat n => Binary (FixLen n ByteString) where
  put (FixLen bs) =
    let len = fromInteger $ natVal (Proxy :: Proxy n)
    in putPaddedByteString $ BS.take len bs
  get = do
    let cap = fromInteger $ natVal (Proxy :: Proxy n)
    len <- getWord32be <&> fromIntegral
    if len /= cap
      then fail $ "Invalid byte length: expected "
                <> show cap <> ", got " <> show len
      else do
        bs <- getByteString len
        skip $ padding 4 len
        pure $ FixLen bs


newtype FixLen (n :: Nat) a
  = FixLen
  { unFixLen :: a
  } deriving (Eq, Show)


instance KnownNat n => Binary (VarLen n Text) where
  put (VarLen t) = put (VarLen (toS t) :: VarLen n ByteString)
  get = get <&> \(VarLen bs :: VarLen n ByteString) -> VarLen (toS bs)


newtype DataValue
  = DataValue (VarLen 64 ByteString)
  deriving (Eq, Show, Binary)

mkDataValue :: ByteString -> Maybe DataValue
mkDataValue bs | BS.length bs <= 64 = Just $ DataValue $ VarLen bs
mkDataValue _  = Nothing

unDataValue :: DataValue -> ByteString
unDataValue (DataValue l) = unVarLen l
