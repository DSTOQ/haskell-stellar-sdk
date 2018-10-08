{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stellar.Client.Lens.Internal where

import Control.Lens.TH
import Stellar.Client.Types
import Stellar.Core.Lens

makeFieldsNoPrefix ''Account
makeFieldsNoPrefix ''AccountFlags
makeFieldsNoPrefix ''Liabilities
makeFieldsNoPrefix ''Balance
makeFieldsNoPrefix ''Trustline
makeFieldsNoPrefix ''Thresholds
makeFieldsNoPrefix ''TransactionDetails
