module JsonProperties
  ( prop_json_roundtrip
  ) where

import Data.Aeson  hiding (String)
import Data.String (String)
import Hedgehog
import Protolude

prop_json_roundtrip
  :: forall a
   . (Show a, ToJSON a, FromJSON a)
   => Gen a -> Property
prop_json_roundtrip gen = property $ do
  it <- forAll gen
  let decodeIt :: LByteString -> Either String a
      decodeIt = eitherDecode
  assert $ isRight $ decodeIt $ encode it
