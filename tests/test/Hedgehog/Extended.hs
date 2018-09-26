module Hedgehog.Extended
  ( module Hedgehog
  , property1
  ) where

import Hedgehog
import Protolude


property1 :: PropertyT IO () -> Property
property1 = withTests 1 . property
