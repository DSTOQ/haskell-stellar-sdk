{-# LANGUAGE TemplateHaskell #-}
module Stellar.Client.Lens where

import Control.Lens.TH
import Stellar.Client.Types

makeFieldsNoPrefix ''AccountFlags
makeFieldsNoPrefix ''AccountDetails
makeFieldsNoPrefix ''Liabilities
makeFieldsNoPrefix ''Balance
makeFieldsNoPrefix ''Thresholds
makeFieldsNoPrefix ''TransactionDetails
