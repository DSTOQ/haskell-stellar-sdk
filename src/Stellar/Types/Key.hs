{-# LANGUAGE StrictData #-}

module Stellar.Types.Key
  ( module Stellar.Types.Key.Secret
  , module Stellar.Types.Key.Public
  , SignatureHint (..)
  , SignerKey (..)
  , SignerKeyType (..)
  , signerKeyType
  , KeyPair
  , keyPair
  , keyPair'
  , generateKeyPair
  ) where

import           Control.Newtype          (Newtype, pack, unpack)
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
import           Stellar.Types.Key.Parser
import           Stellar.Types.Key.Public
import           Stellar.Types.Key.Secret


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
