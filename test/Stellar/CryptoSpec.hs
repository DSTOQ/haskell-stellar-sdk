module Stellar.CryptoSpec (run) where

import           Control.Lens
import           Hedgehog
import           Protolude
import           Stellar.Crypto
import           Stellar.Gen
import qualified Stellar.Lenses as L


run :: IO Bool
run = checkParallel $ Group "Stellar.CryptoSpec" [
    ("prop_sign", prop_sign)
  ]

prop_sign :: Property
prop_sign = property $ do
  keys <- forAll genKeyPair
  tx <- forAll genTransactionEnvelope
  network <- forAll genNetwork
  let tx' = signTransactionEnvelope network keys tx
  length (tx' ^. L.signatures) === length (tx ^. L.signatures) + 1
