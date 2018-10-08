module Stellar.Core.Types.JsonSpec (run) where

import Hedgehog
import Protolude

import JsonProperties (prop_json_roundtrip)
import Stellar.Gens   (genTransactionEnvelope)

run :: IO Bool
run = checkParallel $ Group "Stellar.Types.JsonSpec"
  [ ("TransactionEnvelope ToJSON <=> FromJSON roundtrip"
    , prop_json_roundtrip genTransactionEnvelope
    )
  ]
