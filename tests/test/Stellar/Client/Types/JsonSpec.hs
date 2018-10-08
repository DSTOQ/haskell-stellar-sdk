module Stellar.Client.Types.JsonSpec (run) where

import Hedgehog
import Protolude
import Stellar.Gens

import JsonProperties (prop_json_roundtrip)

run :: IO Bool
run = checkParallel $ Group "Stellar.Types.JsonSpec"
  [
  -- ("Account ToJSON <=> FromJSON roundtrip", prop_json_roundtrip genAccount)
  ]
