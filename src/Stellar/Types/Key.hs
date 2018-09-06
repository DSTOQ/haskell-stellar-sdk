{-# LANGUAGE StrictData #-}

module Stellar.Types.Key where

import           Control.Monad          (fail)
import qualified Crypto.Error           as CE
import qualified Crypto.PubKey.Ed25519  as ED
import           Crypto.Random.Types    (MonadRandom)
import           Data.Binary.Extended
import           Data.Binary.Get        (label, getByteString)
import qualified Data.ByteArray         as BA
import qualified Data.ByteString.Extended        as BS
import           Data.Word.Extended     (Word32, word32FromBytes, word32ToBytes)
import           Prelude                (show)
import           Protolude              hiding (get, put, show)

newtype SignatureHint
  = SignatureHint
  { _signatureHint :: Word32
  } deriving (Eq, Binary)

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

keyPair :: ED.SecretKey -> ED.PublicKey -> KeyPair
keyPair sk pk = KeyPair (SecretKeyEd25519 sk) (PublicKeyEd25519 pk) hint
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

keyPair' :: ED.SecretKey -> KeyPair
keyPair' sk = keyPair sk (ED.toPublic sk)

generateKeyPair :: MonadRandom m => m KeyPair
generateKeyPair = keyPair' <$> ED.generateSecretKey


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
