module Stellar.Key.ParserSpec where

import           Hedgehog
import           Protolude
import           Stellar.Key.Gens
import           Stellar.Key.Parser

run :: IO Bool
run = checkParallel $ Group "Parser Properties"
  [ ("Parse public key", prop_parse_public_key)
  , ("Parse secret key", prop_parse_secret_key)
  ]

prop_parse_public_key :: Property
prop_parse_public_key = property $ do
  PublicKeyText key <- forAll genPublicKeyText
  void (parsePublicKey key) === Right ()

prop_parse_secret_key :: Property
prop_parse_secret_key = property $ do
  SecretKeyText key <- forAll genSecretKeyText
  void (parseSecretKey key) === Right ()
