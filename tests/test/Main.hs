module Main where

import           Protolude

import           Prelude                         (and)
import           System.Exit                     (exitFailure)

import qualified ParsingSpec
import qualified Stellar.Client.Types.JsonSpec   as ClientJsonSpec
import qualified Stellar.Core.Key.ParserSpec     as ParserSpec
import qualified Stellar.Core.Key.PrinterSpec    as PrinterSpec
import qualified Stellar.Core.Types.BinarySpec   as TypesBinarySpec
import qualified Stellar.Core.Types.JsonSpec     as CoreJsonSpec
import qualified Stellar.Core.Types.ShowReadSpec as TypesShowReadSpec
import qualified Stellar.CryptoSpec              as CryptoSpec


main :: IO ()
main = unlessM (and <$> sequence specs) exitFailure where
  specs =
    [ CoreJsonSpec.run
    , ClientJsonSpec.run
    , TypesBinarySpec.run
    , TypesShowReadSpec.run
    , CryptoSpec.run
    , ParserSpec.run
    , PrinterSpec.run
    , ParsingSpec.run
    ]
