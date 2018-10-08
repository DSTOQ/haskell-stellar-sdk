{-# LANGUAGE StrictData #-}

module Stellar.Core.Types.Internal where

import           Control.Monad               (fail)
import           Control.Newtype             (Newtype, pack, unpack)
import           Crypto.Error                (eitherCryptoError)
import qualified Crypto.PubKey.Ed25519       as ED
import           Data.Aeson
import           Data.Binary.Extended
import           Data.Binary.Get             (getByteString, getWord32be, skip)
import           Data.Binary.Put             (putWord32be)
import qualified Data.ByteArray              as BA
import           Data.ByteString.Base64.Type (getByteString64)
import qualified Data.ByteString.Extended    as BS
import           Data.Foldable               (length)
import           GHC.Exts                    (fromList)
import           GHC.TypeLits
import           Prelude                     (String, show)
import           Protolude                   hiding (get, put, putByteString,
                                              show)

newtype VarLen (n :: Nat) a
  = VarLen
  { unVarLen :: a
  } deriving (Eq, Show)

instance Newtype (VarLen n a) a where
  pack = VarLen
  unpack = unVarLen

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
  deriving (Eq, Binary)

instance Newtype DataValue (VarLen 64 ByteString) where
  pack = DataValue
  unpack (DataValue vl) = vl

printDataValue :: DataValue -> Text
printDataValue = BS.printByteStringBase64 . unpack . unpack

instance Show DataValue where
  show = toS . printDataValue

instance ToJSON DataValue where
  toJSON v = Object $ fromList [("value", String $ printDataValue v)]

instance FromJSON DataValue where
  parseJSON = withObject "Data Value" $ \o -> do
    base64 <- o .: "value"
    either (fail . show) pure $ mkDataValue $ getByteString64 base64

mkDataValue :: ByteString -> Either String DataValue
mkDataValue bs | BS.length bs <= 64 = Right $ DataValue $ VarLen bs
mkDataValue _  = Left "DataValue exceeds 64 bytes"

unDataValue :: DataValue -> ByteString
unDataValue (DataValue l) = unVarLen l
