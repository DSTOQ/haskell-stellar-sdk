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
import           Data.Binary.Put       (putWord32be)
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
    in putVarLenByteString maximumLength bs

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
  put (VarLen s) =
    let maximumLength = fromInteger $ natVal (Proxy :: Proxy n)
    in putVarLenByteString maximumLength (BA.convert s)
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


newtype DataValue
  = DataValue (VarLen 64 ByteString)
  deriving (Eq, Show, Binary)

mkDataValue :: ByteString -> Maybe DataValue
mkDataValue bs | BS.length bs <= 64 = Just $ DataValue $ VarLen bs
mkDataValue _  = Nothing

unDataValue :: DataValue -> ByteString
unDataValue (DataValue l) = unVarLen l
