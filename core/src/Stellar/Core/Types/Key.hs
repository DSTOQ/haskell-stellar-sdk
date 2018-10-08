{-# LANGUAGE StrictData #-}

module Stellar.Core.Types.Key
  ( module Stellar.Core.Types.Key.Secret
  , module Stellar.Core.Types.Key.Public
  , SignatureHint (..)
  , SignerKey (..)
  , SignerKeyType (..)
  , signerKeyType
  , KeyPair
  , keyPair
  , keyPair'
  , generateKeyPair
  ) where

import           Control.Monad                 (fail)
import           Control.Newtype               (Newtype, pack, unpack)
import qualified Crypto.PubKey.Ed25519         as ED
import           Crypto.Random.Types           (MonadRandom)
import           Data.Aeson                    (FromJSON, ToJSON,
                                                Value (String), parseJSON,
                                                toJSON, withText)
import           Data.Binary.Extended
import           Data.Binary.Get               (label)
import qualified Data.ByteArray                as BA
import qualified Data.ByteString.Extended      as BS
import           Data.Word.Extended            (Word32, word32FromBytes,
                                                word32ToBytes)
import           Prelude                       (show)
import           Protolude                     hiding (get, put, show)
import           Stellar.Core.Types.Key.Parser
import           Stellar.Core.Types.Key.Public
import           Stellar.Core.Types.Key.Secret
import           Stellar.Core.Types.Sha256
import           Text.Read.Extended            (Lexeme (Ident))
import qualified Text.Read.Extended            as R



newtype SignatureHint
  = SignatureHint
  { _signatureHint :: Word32
  } deriving (Eq, Binary)

instance Newtype SignatureHint Word32 where
  pack = SignatureHint
  unpack = _signatureHint

instance Show SignatureHint where
  show (SignatureHint w) = "SignatureHint {"
    <> "_signatureHint = "
    <> show (BS.printByteStringBase16 $ BS.pack $ word32ToBytes w)
    <> "}"

data KeyPair
  = KeyPair
  { _secretKey :: SecretKey
  , _publicKey :: PublicKey
  , _hint      :: SignatureHint
  } deriving (Eq)

instance Show KeyPair where
  show (KeyPair sk pk h) = "KeyPair {"
    <> "_secretKey = " <> show sk
    <> ", _publicKey = " <> show (BS.printByteStringBase16 $ BA.convert pk)
    <> ", _hint = " <> show h
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
  deriving (Eq, Enum, Bounded)

printSignerKeyType :: SignerKeyType -> Text
printSignerKeyType = \case
  SignerKeyTypeEd25519   -> "ed25519_public_key"
  SignerKeyTypePreAuthTx -> "sha256_hash"
  SignerKeyTypeHashX     -> "preauth_tx"

instance Show SignerKeyType where
  show = toS . printSignerKeyType

instance Read SignerKeyType where
  readPrec = R.lexP >>= \case
    Ident "ed25519_public_key" -> pure SignerKeyTypeEd25519
    Ident "sha256_hash"        -> pure SignerKeyTypePreAuthTx
    Ident "preauth_tx"         -> pure SignerKeyTypeHashX
    t                          -> fail $ "Invalid SignerKeyType: " <> show t
  readListPrec = R.readListPrecDefault

instance ToJSON SignerKeyType where
  toJSON = String . printSignerKeyType

instance FromJSON SignerKeyType where
  parseJSON = withText "Signer Key Type" $
    either (fail . show) pure . readEither . toS

instance Binary SignerKeyType where
  get = label "SignerKeyType" getEnum
  put = putEnum

signerKeyType :: SignerKey -> SignerKeyType
signerKeyType = \case
  SignerKeyEd25519 _   -> SignerKeyTypeEd25519
  SignerKeyPreAuthTx _ -> SignerKeyTypePreAuthTx
  SignerKeyHashX _     -> SignerKeyTypeHashX


data SignerKey
  = SignerKeyEd25519 PublicKey
  | SignerKeyPreAuthTx Sha256
  | SignerKeyHashX Sha256
  deriving (Eq, Show)

instance Binary SignerKey where
  get = label "SignerKey" $ get >>= \case
    SignerKeyTypeEd25519   -> SignerKeyEd25519   <$> get
    SignerKeyTypePreAuthTx -> SignerKeyPreAuthTx <$> get
    SignerKeyTypeHashX     -> SignerKeyHashX     <$> get
  put (SignerKeyEd25519 k)   = put SignerKeyTypeEd25519   >> put k
  put (SignerKeyPreAuthTx h) = put SignerKeyTypePreAuthTx >> put h
  put (SignerKeyHashX h)     = put SignerKeyTypeHashX     >> put h
