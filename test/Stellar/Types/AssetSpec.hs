module Stellar.Types.AssetSpec (run) where

import Data.String  (String)
import Hedgehog
import Protolude
import Stellar.Gens

run :: IO Bool
run = checkParallel $ Group "Stellar.Types.AssetSpec"
  [ ("AssetCode Show <=> Read roundtrip"
    , prop_roundtrip_asset_code
    )
  , ("PreciseAssetType Show <=> Read roundtrip"
    , prop_roundtrip_precise_asset_type
    )
  ]

prop_roundtrip_asset_code :: Property
prop_roundtrip_asset_code = prop_roundtrip genAssetCode

prop_roundtrip_precise_asset_type :: Property
prop_roundtrip_precise_asset_type = prop_roundtrip genPreciseAssetType

prop_roundtrip :: forall a. (Show a, Read a) => Gen a -> Property
prop_roundtrip gen = property $ do
  it <- forAll gen
  let readIt :: String -> Maybe a
      readIt = readMaybe
  assert $ isJust $ readIt $ show it
