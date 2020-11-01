{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Shared ( App (..) ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger    ( MonadLogger (..), MonadTime (..) )
import Infrastructure.Requester ( MonadRequester (..) )

import Control.Monad.Catch      ( MonadCatch, MonadThrow )
import Control.Monad.Reader     ( MonadReader, ReaderT (..) )
import Control.Monad.IO.Class   ( MonadIO, liftIO )
import Data.Time                ( getCurrentTime )
import Network.HTTP.Client      ( httpLbs )

import qualified Data.Text.IO as TextIO

-- TYPES AND INSTANCES -----------------------------------------------------

newtype App r a = App { unApp :: ReaderT r IO a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadCatch
                   , MonadIO
                   , MonadReader r
                   , MonadThrow
                   )

instance MonadLogger (App r) where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime (App r) where
  getTime = liftIO getCurrentTime

instance MonadRequester (App r) where
  requester manager req = liftIO $ httpLbs req manager
