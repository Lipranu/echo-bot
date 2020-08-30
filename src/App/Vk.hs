{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module App.Vk ( Config, mkApp, runApp ) where

import           Internal (Lock, Has (..))
import qualified Infrastructure.Logger    as Logger
import qualified Infrastructure.Requester as Requester

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT )
import Control.Exception      ( try )
import Data.Aeson             ( (.:) )
import Data.Text              ( Text )
import Data.Time              ( getCurrentTime )
import Network.HTTP.Client    ( httpLbs )

import qualified Data.Aeson   as Aeson
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

data Config = Config
  { cToken :: Token
  , cGroup :: Group
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Vk.Config" $ \o -> Config
    <$> (Token <$> o .: "token")
    <*> (Group <$> o .: "group_id")

newtype Token = Token { unToken :: Text }

newtype Group = Group { unGroup :: Text }

data Env = Env
  { token  :: Token
  , group  :: Group
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
  requester manager req = liftIO $ try $ httpLbs req manager

app :: App ()
app = do
  Logger.logDebug ("Debug Message" :: Text)
  Logger.logInfo ("Info Message" :: Text)
  Logger.logWarning ("Warning Message" :: Text)
  Logger.logError ("Error Message" :: Text)

mkApp :: Config -> Logger.Config -> Lock -> Env
mkApp Config {..} cLogger = Env cToken cGroup logger
  where logger = Logger.mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)
