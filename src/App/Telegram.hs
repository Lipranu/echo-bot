{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Telegram ( Config, mkApp, runApp ) where

import           Internal                 ( Lock, Has (..) )
import           Infrastructure.Logger    ( Logger )
import           Infrastructure.Requester ( Requester )

import qualified Infrastructure.Logger    as Logger
import qualified Infrastructure.Requester as Requester

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT )
import Control.Exception      ( try )
import Data.Aeson             ( (.:) )
import Data.Text              ( Text )
import Data.Time              ( getCurrentTime )

import qualified Network.HTTP.Client as HTTP
import qualified Data.Aeson          as Aeson
import qualified Data.Text           as Text
import qualified Data.Text.IO        as TextIO

data Config = Config
  { cToken :: Token
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Telegram.Config" $ \o -> Config
    <$> (Token <$> o .: "token")

newtype Token = Token { unToken :: Text }

data Env = Env
  { envToken     :: Token
  , envLogger    :: Logger App
  , envLock      :: Lock
  , envRequester :: Requester App
  }

instance Has Lock Env where
  getter = envLock

instance Has (Logger App) Env where
  getter = envLogger

instance Has (Requester App) Env where
  getter = envRequester

newtype App a = App { unApp :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance Logger.MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance Logger.MonadTime App where
  getTime = liftIO getCurrentTime

instance Requester.MonadRequester App where
  requester manager req = liftIO $ try $ HTTP.httpLbs req manager

app :: App ()
app = do
  Logger.logDebug ("Debug Message" :: Text)
  Logger.logInfo ("Info Message" :: Text)
  Logger.logWarning ("Warning Message" :: Text)
  Logger.logError ("Error Message" :: Text)

mkApp :: Config -> Logger.Config -> Lock -> HTTP.Manager -> Env
mkApp Config {..} cLogger lock = Env cToken logger lock
                               . Requester.mkRequester
  where logger = Logger.mkLogger cLogger "Telegram"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)
