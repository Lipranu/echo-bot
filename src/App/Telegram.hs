{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Telegram ( Config, mkApp, runApp ) where

import           Internal        ( Lock, Has (..) )
import qualified Logger
import qualified Requester

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT )
import Data.Aeson             ( (.:) )
import Data.Text              ( Text )
import Data.Time              ( getCurrentTime )
import Network.HTTP.Client    ( httpLbs )

import qualified Data.Aeson   as Aeson
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

data Config = Config
  { cToken :: Token
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Telegram.Config" $ \o -> Config
    <$> (Token <$> o .: "token")

newtype Token = Token { unToken :: Text }

data Env = Env
  { token  :: Token
  , logger :: Logger.Logger App
  , lock   :: Lock
  }

instance Has Lock Env where
  getter = lock

instance Has (Logger.Logger App) Env where
  getter = logger

newtype App a = App { unApp :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance Logger.MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance Logger.MonadTime App where
  getTime = liftIO getCurrentTime

instance Requester.MonadRequester App where
  request manager = liftIO . httpLbs manager

app :: App ()
app = do
  Logger.logDebug ("Debug Message" :: Text)
  Logger.logInfo ("Info Message" :: Text)
  Logger.logWarning ("Warning Message" :: Text)
  Logger.logError ("Error Message" :: Text)

mkApp :: Config -> Logger.Config -> Lock -> Env
mkApp Config {..} cLogger = Env cToken logger
  where logger = Logger.mkLogger cLogger "Telegram"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)
