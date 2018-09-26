module Hedgehog.Gen.Extended
  ( module Hedgehog.Gen
  , expWord32
  , expWord64
  , expInt32
  , expInt64
  , either
  ) where

import           Protolude      hiding (either)

import           Hedgehog
import           Hedgehog.Gen
import qualified Hedgehog.Range as Range

either :: MonadGen m => m a -> m b -> m (Either a b)
either a b = choice [fmap Left a, fmap Right b]

expWord32 :: Gen Word32
expWord32 = word32 Range.exponentialBounded

expWord64 :: Gen Word64
expWord64 = word64 Range.exponentialBounded

expInt32 :: Gen Int32
expInt32 = int32 Range.exponentialBounded

expInt64 :: Gen Int64
expInt64 = int64 Range.exponentialBounded
