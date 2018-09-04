module Stellar.Key.PrinterSpec where

import           Hedgehog
import           Protolude
import           Stellar.Key.Gens
import           Stellar.Key.Parser
import           Stellar.Key.Printer

run :: IO Bool
run = checkParallel $ Group "Printer Properties"
  [ ("Roundtrip public key", prop_roundtrip_public_key)
  , ("Roundtrip secret key", prop_roundtrip_secret_key)
  ]

prop_roundtrip_public_key :: Property
prop_roundtrip_public_key = property $ do
  PublicKeyText key <- forAll genPublicKeyText
  (printPublicKey <$> parsePublicKey key) === Right key

prop_roundtrip_secret_key :: Property
prop_roundtrip_secret_key = property $ do
  SecretKeyText key <- forAll genSecretKeyText
  (printSecretKey <$> parseSecretKey key) === Right key
