module Stellar.CryptoSpec (run) where

import           Control.Lens
import           Hedgehog
import           Protolude
import           Stellar.Crypto
import           Stellar.Gen
import qualified Stellar.Lenses as L
import           Stellar.Types



run :: IO Bool
run = checkParallel $ Group "Stellar.CryptoSpec" [
    ("prop_sign", prop_sign)
  ]

prop_sign :: Property
prop_sign = property $ do
  kp <- forAll genKeyPair
  tx <- forAll genTransactionEnvelope
  let tx' = signTransactionEnvelope Testnet kp tx
  length (tx' ^. L.signatures) === length (tx ^. L.signatures) + 1
