module Control.Monad.Trans.Stellar
  ( MonadStellar (..)
  , StellarT (..)
  , runStellarT
  , ApiBase (..)
  ) where

import           Named
import           Protolude             hiding (get)
import           Stellar.Client.Types

import           Control.Monad.Catch   (MonadThrow)
import           Control.Monad.Rest    (ApiBase (..), MonadRest, relativeRes)
import           Control.Monad.Trans.Rest    (RestT (..), runRestT)
import qualified Control.Monad.Rest    as Rest
import           Control.Monad.Stellar (MonadStellar (..))
import           Control.Monad.Trans   (MonadTrans)
import           Control.Newtype       (Newtype, pack, unpack)
import           Web.HttpApiData       (ToHttpApiData (..))

newtype StellarT m a
  = StellarT
  { runStellarT :: m a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadThrow
  , MonadIO
  , MonadRest
  )

instance MonadTrans StellarT where
  lift = StellarT

instance Newtype (StellarT m a) (m a) where
  unpack = runStellarT
  pack = StellarT

instance (MonadIO m, MonadThrow m) => MonadStellar (StellarT (RestT m)) where
  accountDetails accountId = StellarT . Rest.get $ relativeRes
    ! #path ["accounts", printAccountId accountId]
    ! defaults

  accountTransactions accountId
    (argDef #cursor Nothing -> cursor)
    (argDef #order  Nothing -> order)
    (argDef #limit  Nothing -> limit)
     = StellarT . Rest.get $ relativeRes
      ! #path ["accounts", printAccountId accountId, "transactions"]
      ! #query query
      ! defaults
      where
        query :: [(Text, Text)]
        query = foldMap maybeToList
          [ ("cursor",). toUrlPiece <$> cursor
          , ("order",) . toUrlPiece <$> order
          , ("limit",) . toUrlPiece <$> limit
          ]
