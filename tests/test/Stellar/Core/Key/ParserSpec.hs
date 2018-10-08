module Stellar.Core.Key.ParserSpec where

import qualified Data.Text              as T
import           Hedgehog.Extended
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Protolude
import           Stellar.Core.Types.Key
import           Stellar.Gens


run :: IO Bool
run = checkParallel $ Group "Parser Properties"
  [ ("Parse public key", prop_parse_public_key)
  , ("Parse secret key", prop_parse_secret_key)
  , ("Parser error for invalid input length", prop_invalid_length_base32)
  , ("Parser error for invalid Base 32", prop_invalid_base32)
  ]

prop_parse_public_key :: Property
prop_parse_public_key = property $ do
  PublicKeyText key <- forAll genPublicKeyText
  (void . evalEither . parsePublicKey) key

prop_parse_secret_key :: Property
prop_parse_secret_key = property $ do
  SecretKeyText key <- forAll genSecretKeyText
  (void . evalEither . parseSecretKey) key

prop_invalid_length_base32 :: Property
prop_invalid_length_base32 = property $ do
  key <- forAll $ mfilter ((/= 72) . T.length)
                $ Gen.text (Range.linear 1 100) Gen.alphaNum
  KeyParser parser <- forAll genKeyParser
  assert $ isLeft $ parser key

prop_invalid_base32 :: Property
prop_invalid_base32 = property $ do
  key <- forAll $ Gen.text (Range.singleton 72) Gen.alphaNum
  KeyParser parser <- forAll genKeyParser
  assert $ isLeft $ parser key
