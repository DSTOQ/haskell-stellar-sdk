{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}


module Stellar.Types.Internal where

import           Control.Monad         (fail)
import           Crypto.Error          (eitherCryptoError)
import qualified Crypto.PubKey.Ed25519 as ED
import           Data.Binary.Extended
import           Data.Binary.Get       (getByteString, getWord32be, skip)
import           Data.Binary.Put       (putByteString, putWord32be)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString       as BS
import           Data.Foldable         (length)
import           GHC.TypeLits
import           Protolude             hiding (get, put, putByteString)


newtype VarLen (n :: Nat) a
  = VarLen
  { unVarLen :: a
  } deriving (Eq, Show)

getVarLen :: forall n a. Binary (VarLen n a) => Proxy n -> Get a
getVarLen _ = fmap unVarLen (get :: Get (VarLen n a))

instance KnownNat n => Binary (VarLen n ByteString) where
  put (VarLen bs) =
    let maximumLength = fromInteger $ natVal (Proxy :: Proxy n)
        actualLength = BS.length bs
    in if actualLength <= maximumLength
          then putPaddedByteString bs
          else fail $ "Attempt to put a longer bytestring: "
                    <> "max length = " <> show maximumLength
                    <> ", actual = " <> show actualLength
  get = do
    len <- getWord32be <&> fromIntegral
    let cap = fromInteger $ natVal (Proxy :: Proxy n)
    if len > cap
      then fail $ "Max length (" <> show cap <> ") exceeded (" <> show len <> ")"
      else do
        bs <- getByteString len
        skip $ padding 4 len
        pure $ VarLen bs

instance KnownNat n => Binary (VarLen n Text) where
  put (VarLen t) = put (VarLen (toS t) :: VarLen n ByteString)
  get = get <&> \(VarLen bs :: VarLen n ByteString) -> VarLen (toS bs)

instance KnownNat n => Binary (VarLen n ED.Signature) where
  put (VarLen s) = putPaddedByteString $ BA.convert s
  get = do
    (VarLen bs :: VarLen n ByteString) <- get
    either (fail . show) (pure . VarLen) $ eitherCryptoError $ ED.signature bs

instance (KnownNat n, Binary b) => Binary (VarLen n [b]) where
  put (VarLen bs) = do
    putWord32be $ fromIntegral $ length bs
    mapM_ put bs
  get = do
    len <- getWord32be <&> fromIntegral
    let cap = fromIntegral $ natVal (Proxy :: Proxy n)
    if len > cap
      then fail $ "Max length (" <> show cap <> ") exceeded (" <> show len <> ")"
      else VarLen <$> replicateM len get


newtype FixLen (n :: Nat) a
  = FixLen
  { unFixLen :: a
  } deriving (Eq, Show)

getFixLen :: forall n a. Binary (FixLen n a) => Proxy n -> Get a
getFixLen _ = fmap unFixLen (get :: Get (FixLen n a))

instance (KnownNat n, Mod n 4 ~ 0) => Binary (FixLen n ByteString) where
  put (FixLen bs) =
    let expectedLen = fromInteger $ natVal (Proxy :: Proxy n)
        actualLen = BS.length bs
    in if actualLen == expectedLen
          then putByteString $ bs <> BS.replicate (padding 4 (BS.length bs)) 0
          else fail $ "Attempt to put a byte string with invalid length: "
                    <> "declared = " <> show expectedLen
                    <> ", actual = " <> show actualLen
  get = FixLen <$> getByteString (fromInteger $ natVal (Proxy :: Proxy n))


newtype DataValue
  = DataValue (VarLen 64 ByteString)
  deriving (Eq, Show, Binary)

mkDataValue :: ByteString -> Maybe DataValue
mkDataValue bs | BS.length bs <= 64 = Just $ DataValue $ VarLen bs
mkDataValue _  = Nothing

unDataValue :: DataValue -> ByteString
unDataValue (DataValue l) = unVarLen l
