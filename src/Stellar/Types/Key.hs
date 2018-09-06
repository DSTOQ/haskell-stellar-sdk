{-# LANGUAGE StrictData #-}

module Stellar.Types.Key where

import           Control.Monad            (fail)
import           Control.Newtype          (Newtype, pack, unpack)
import qualified Crypto.Error             as CE
import qualified Crypto.PubKey.Ed25519    as ED
import           Crypto.Random.Types      (MonadRandom)
import           Data.Binary.Extended
import           Data.Binary.Get          (getByteString, label)
import qualified Data.ByteArray           as BA
import qualified Data.ByteString.Extended as BS
import           Data.Word.Extended       (Word32, word32FromBytes,
                                           word32ToBytes)
import           Prelude                  (show)
import           Protolude                hiding (get, put, show)

newtype SignatureHint
  = SignatureHint
  { _signatureHint :: Word32
  } deriving (Eq, Binary)

instance Newtype SignatureHint Word32 where
  pack = SignatureHint
  unpack = _signatureHint

instance Show SignatureHint where
  show (SignatureHint w) = "SignatureHint "
    <> BS.showByteString (BS.pack $ word32ToBytes w)


data KeyPair
  = KeyPair
  { _secretKey :: SecretKey
  , _publicKey :: PublicKey
  , _hint      :: SignatureHint
  } deriving (Eq)

instance Show KeyPair where
  show (KeyPair sk pk h) = "KeyPair {"
    <> "_secretKey = " <> Prelude.show sk
    <> ", _publicKey = " <> BS.showByteString (BA.convert pk)
    <> ", _hint = " <> Prelude.show h
    <> "}"

keyPair :: SecretKey -> PublicKey -> KeyPair
keyPair sk pk = KeyPair sk pk hint
  where
    hint :: SignatureHint
    hint = SignatureHint $ word32FromBytes $ takeR 4 $ BA.unpack pk

    takeR :: Int -> [a] -> [a]
    takeR n l = go (drop n l) l
      where
        go :: [a] -> [a] -> [a]
        go [] r          = r
        go (_:xs) (_:ys) = go xs ys
        go _ []          = []

keyPair' :: SecretKey -> KeyPair
keyPair' sk@(SecretKeyEd25519 edsk) =
  keyPair sk $ PublicKeyEd25519 $ ED.toPublic edsk

generateKeyPair :: MonadRandom m => m KeyPair
generateKeyPair = keyPair' . pack <$> ED.generateSecretKey


data PublicKeyType
  = PublicKeyTypeEd25519
  deriving (Eq, Show, Enum, Bounded)

instance Binary PublicKeyType where
  get = label "PublicKeyType" getEnum
  put = putEnum


newtype PublicKey
  = PublicKeyEd25519
  { _publicKeyEd25519 :: ED.PublicKey
  } deriving (Eq, BA.ByteArrayAccess)

instance Newtype PublicKey ED.PublicKey where
  pack = PublicKeyEd25519
  unpack = _publicKeyEd25519

instance Show PublicKey where
  show (PublicKeyEd25519 pk) =
    "PublicKeyEd25519 {_publicKeyEd25519 = "
    <> BS.showByteString (BA.convert pk) <> "}"

instance Binary PublicKey where
  put (PublicKeyEd25519 edPk) = do
    put PublicKeyTypeEd25519
    putFixLenByteString 32 $ BA.convert edPk
  get = label "PublicKey" $ get
    >>= \case PublicKeyTypeEd25519 -> do
                bs <- getByteString 32
                key <- ED.publicKey bs & CE.onCryptoFailure (fail . show) pure
                pure $ PublicKeyEd25519 key

newtype SecretKey
  = SecretKeyEd25519
  { _secretKeyEd25519 :: ED.SecretKey
  } deriving (Eq, Show, BA.ByteArrayAccess)

instance Newtype SecretKey ED.SecretKey where
  pack = SecretKeyEd25519
  unpack = _secretKeyEd25519

data SignerKeyType
  = SignerKeyTypeEd25519
  | SignerKeyTypePreAuthTx
  | SignerKeyTypeHashX
  deriving (Eq, Show, Enum, Bounded)

instance Binary SignerKeyType where
  get = label "SignerKeyType" getEnum
  put = putEnum

signerKeyType :: SignerKey -> SignerKeyType
signerKeyType = \case
  SignerKeyEd25519 _   -> SignerKeyTypeEd25519
  SignerKeyPreAuthTx _ -> SignerKeyTypePreAuthTx
  SignerKeyHashX _     -> SignerKeyTypeHashX


data SignerKey
  = SignerKeyEd25519 ByteString
  | SignerKeyPreAuthTx ByteString
  | SignerKeyHashX ByteString
  deriving (Eq, Show)

instance Binary SignerKey where
  get = label "SignerKey" $ do
    keyType <- get
    let constructor = case keyType of
          SignerKeyTypeEd25519   -> SignerKeyEd25519
          SignerKeyTypePreAuthTx -> SignerKeyPreAuthTx
          SignerKeyTypeHashX     -> SignerKeyHashX
    constructor <$> getByteString 32
  put (SignerKeyEd25519 bs) =
    put SignerKeyTypeEd25519 >> putFixLenByteString 32 bs
  put (SignerKeyPreAuthTx bs) =
    put SignerKeyTypePreAuthTx >> putFixLenByteString 32 bs
  put (SignerKeyHashX bs) =
    put SignerKeyTypeHashX >> putFixLenByteString 32 bs
