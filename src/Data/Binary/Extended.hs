{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Binary.Extended
  ( module Data.Binary
  , getEnum
  , putEnum
  , Padded (..)
  , putVarLenByteString
  , putFixLenByteString
  , padding
  ) where

import           Control.Monad   (fail)
import           Data.Binary
import           Data.Binary.Get (getWord32be)
import           Data.Binary.Put (putByteString, putWord32be)
import           Data.ByteString as BS
import           Protolude       hiding (get, put, putByteString)

getEnum :: Enum a => Get a
getEnum = getWord32be <&> toEnum . fromIntegral

putEnum :: Enum a => a -> Put
putEnum = putWord32be . fromIntegral . fromEnum

padding :: Int -> Int -> Int
padding p l = m - l
  where
  m = p * if r == 0 then q else q + 1
  (q, r) = l `quotRem` p

putVarLenByteString :: Int -> ByteString -> Put
putVarLenByteString cap bs = do
  let len = BS.length bs
  if len <= cap
    then putWord32be (fromIntegral len)
      >> putByteString (bs <> BS.replicate (padding 4 len) 0)
    else fail $ "ByteString actual length (" <> show len
             <> ") exceeds declared length " <> show cap

putFixLenByteString :: Int -> ByteString -> Put
putFixLenByteString cap bs
  | BS.length bs == cap = putByteString bs
  | otherwise = fail $ "ByteString actual length (" <> show (BS.length bs)
                  <> ") differs from declared length " <> show cap


newtype Padded a
  = Padded
  { unPadded :: a
  } deriving (Eq, Show, Functor)

instance Binary (Padded Bool) where
  get = getWord32be <&> (> 0) <&> Padded
  put (Padded True)  = putWord32be 1
  put (Padded False) = putWord32be 0


instance Binary a => Binary (Padded (Maybe a)) where
  put (Padded Nothing)  = putWord32be 0
  put (Padded (Just a)) = putWord32be 1 >> put a
  get = do
    w <- getWord32be
    case w of
      0 -> pure $ Padded Nothing
      _ -> Padded . Just <$> get
