
{-# LANGUAGE StrictData #-}

module Stellar.Core.Types.Key.Secret
  ( SecretKey (..)
  , parseSecretKey
  , printSecretKey
  , Error (..)
  ) where

import           Control.Newtype                (Newtype, pack, unpack)
import qualified Crypto.PubKey.Ed25519          as ED
import qualified Data.ByteArray                 as BA
import           Protolude                      hiding (get, put, show)
import           Stellar.Core.Types.Key.Parser
import           Stellar.Core.Types.Key.Printer
import           Stellar.Core.Types.Key.Version (KeyVersion (Seed))


newtype SecretKey
  = SecretKeyEd25519
  { _secretKeyEd25519 :: ED.SecretKey
  } deriving (Eq, Show, BA.ByteArrayAccess)

instance Newtype SecretKey ED.SecretKey where
  pack = SecretKeyEd25519
  unpack = _secretKeyEd25519

parseSecretKey :: Text -> Either Error SecretKey
parseSecretKey = fmap SecretKeyEd25519 . fromText Seed ED.secretKey

printSecretKey :: SecretKey -> Text
printSecretKey = toText Seed . BA.convert
