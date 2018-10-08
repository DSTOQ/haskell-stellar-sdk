module Stellar.Core.Types.ShowReadSpec where

import Data.String  (String)
import Hedgehog
import Protolude
import Stellar.Gens

run :: IO Bool
run = checkParallel $ Group "Show <=> Read roundtrip"
  [ ("AssetCode", roundtrip genAssetCode)
  , ("PreciseAssetType", roundtrip genPreciseAssetType)
  , ("OfferId", roundtrip genOfferId)
  , ("Fee", roundtrip genFee)
  , ("NonNegativeInt64", roundtrip genNonNegativeInt64)
  , ("SequenceNumber", roundtrip genSequenceNumber)
  , ("HomeDomain", roundtrip genHomeDomain)
  ]

roundtrip :: forall a. (Eq a, Show a, Read a) => Gen a -> Property
roundtrip gen = property $ do
  it <- forAll gen
  let readIt :: String -> Either String a
      readIt = readEither
  readIt (show it) === Right it
