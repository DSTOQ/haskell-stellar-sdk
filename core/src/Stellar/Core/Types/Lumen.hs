module Stellar.Core.Types.Lumen
  ( XLM (..)
  , Stroop (..)
  , xlmToStroop
  , stroopToXlm
  , xlm
  ) where

import Control.Newtype (Newtype, pack, unpack)
import Data.Aeson      (FromJSON, ToJSON)
import Data.Binary     (Binary)
import Protolude


newtype Stroop
  = Stroop Int64
  deriving (Eq, Ord, Show, Enum, Num, Binary, Read, FromJSON, ToJSON)

instance Newtype Stroop Int64 where
  pack = Stroop
  unpack (Stroop i) = i

newtype XLM
  = XLM Stroop
  deriving (Eq, Ord, Show, Enum, Num, Binary)

instance Newtype XLM Int64 where
  pack = stroopToXlm . pack
  unpack = unpack . xlmToStroop


xlm :: Int64 -> XLM
xlm i = pack (10000000 * i)

xlmToStroop :: XLM -> Stroop
xlmToStroop (XLM stroop) = stroop

stroopToXlm :: Stroop -> XLM
stroopToXlm = XLM
