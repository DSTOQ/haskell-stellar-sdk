module Stellar.Types.JsonSpec (run) where

import Data.Aeson   hiding (String)
import Data.String  (String)
import Hedgehog
import Protolude
import Stellar.Gens (genTransactionEnvelope)


run :: IO Bool
run = checkParallel $ Group "Stellar.Types.JsonSpec"
  [ ("TransactionEnvelope ToJSON <=> FromJSON roundtrip"
    , prop_json_roundtrip_tx_envelope
    )
  ]

prop_json_roundtrip_tx_envelope :: Property
prop_json_roundtrip_tx_envelope = prop_json_roundtrip genTransactionEnvelope

prop_json_roundtrip
  :: forall a
   . (Show a, ToJSON a, FromJSON a)
   => Gen a -> Property
prop_json_roundtrip gen = property $ do
  it <- forAll gen
  let decodeIt :: LByteString -> Either String a
      decodeIt = eitherDecode
  assert $ isRight $ decodeIt $ encode it
