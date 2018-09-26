module Text.Read.Extended
  ( module Text.Read
  , digit
  , lowerAz
  , upperAz
  ) where

import Control.Monad (mfilter)
import Data.Char
import Protolude     hiding (get)
import Text.Read


digit :: ReadPrec Char
digit = mfilter isDigit get

-- | 'a' to 'z'
lowerAz :: ReadPrec Char
lowerAz = mfilter (az 'a') get

-- | 'A' to 'Z'
upperAz :: ReadPrec Char
upperAz = mfilter (az 'A') get

az :: Char -> Char -> Bool
az a c = (fromIntegral (ord c - ord a) :: Word) <= 25
