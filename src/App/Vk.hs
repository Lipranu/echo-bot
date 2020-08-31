{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module App.Vk ( Config, mkApp, runApp ) where

import           Internal                 ( Lock, Has (..) )
import           Infrastructure.Logger    ( Logger )
import           Infrastructure.Requester ( Requester )

import qualified Infrastructure.Logger    as Logger
import qualified Infrastructure.Requester as Requester

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT, asks )
import Control.Exception      ( try )
import Data.Aeson             ( (.:) )
import Data.Text              ( Text )
import Data.Text.Encoding     ( encodeUtf8 )
import Data.Time              ( getCurrentTime )
import GHC.Generics           ( Generic )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text           as Text
import qualified Data.Text.IO        as TextIO
import qualified Network.HTTP.Client as HTTP

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
  { envToken     :: Token
  , envGroup     :: Group
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

instance Has Token Env where
  getter = envToken

instance Has Group Env where
  getter = envGroup

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
  r <- getLongPollServer >>= Requester.requestAndDecode @GetServer
  case r of
    Requester.Result v -> Logger.logDebug $ show v
    Requester.DecodeError e v -> Logger.logError e >> Logger.logDebug v
    Requester.RequestError e -> Logger.logError e
--  Logger.logInfo ("Info Message" :: Text)
--  Logger.logWarning ("Warning Message" :: Text)
--  Logger.logError ("Error Message" :: Text)

mkApp :: Config -> Logger.Config -> Lock -> HTTP.Manager -> Env
mkApp Config {..} cLogger lock = Env cToken cGroup logger lock
                               . Requester.mkRequester
  where logger = Logger.mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getLongPollServer :: App GetLongPollServer
getLongPollServer = GetLongPollServer
  <$> asks getter
  <*> asks getter

data GetLongPollServer = GetLongPollServer Token Group

instance Requester.ToRequest GetLongPollServer where
  toRequest (GetLongPollServer (Token t) (Group g))
    = mkBody $ defaultRequest { HTTP.path = "/method/groups.getLongPollServer" }
    where mkBody = HTTP.urlEncodedBody
                     [ ("access_token", encodeUtf8 t)
                     , ("group_id", encodeUtf8 g)
                     , ("v", "5.122")
                     ]

data GetServer = GetServer
  { guKey :: Text
  , guTs  :: Text
  , guServer :: Text
  } deriving (Generic, Show)

instance Aeson.FromJSON GetServer where
  parseJSON = Aeson.parseJsonDrop

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }
