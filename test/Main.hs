module Main where

import           Prelude                 (and)
import           Protolude
import qualified Stellar.CryptoSpec      as CryptoSpec
import qualified Stellar.Key.ParserSpec  as ParserSpec
import qualified Stellar.Key.PrinterSpec as PrinterSpec
import qualified Stellar.TypesSpec       as TypesSpec
import           System.Exit             (exitFailure)


main :: IO ()
main = unlessM (and <$> sequence specs) exitFailure where
  specs =
    [ TypesSpec.run
    , CryptoSpec.run
    , ParserSpec.run
    , PrinterSpec.run
    ]
