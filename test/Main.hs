module Main where

import           Protolude
import qualified Stellar.CryptoSpec as CryptoSpec
import qualified Stellar.TypesSpec  as TypesSpec
import           System.Exit        (exitFailure)


main :: IO ()
main = do
  unlessM TypesSpec.run exitFailure
  unlessM CryptoSpec.run exitFailure
  pure ()
