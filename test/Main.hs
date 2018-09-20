module Main where

import           Prelude                  (and)
import           Protolude
import qualified Stellar.CryptoSpec       as CryptoSpec
import qualified Stellar.Key.ParserSpec   as ParserSpec
import qualified Stellar.Key.PrinterSpec  as PrinterSpec
import qualified Stellar.Types.AssetSpec  as AssetSpec
import qualified Stellar.Types.BinarySpec as TypesBinarySpec
import qualified Stellar.Types.JsonSpec   as TypesJsonSpec
import           System.Exit              (exitFailure)


main :: IO ()
main = unlessM (and <$> sequence specs) exitFailure where
  specs =
    [ TypesJsonSpec.run
    , TypesBinarySpec.run
    , AssetSpec.run
    , CryptoSpec.run
    , ParserSpec.run
    , PrinterSpec.run
    ]
