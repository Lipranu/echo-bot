{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS ---------------------------------------------------------------------

import Infrastructure.Logger    hiding ( Config )
import Infrastructure.Requester
import Internal

import qualified Infrastructure.Logger as Logger

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT, asks )
import Control.Exception      ( try )
import Data.Aeson             ( (.:) )
import Data.Bifunctor         ( bimap )
import Data.Text              ( Text )
import Data.Maybe             ( fromMaybe )
import Data.Text.Encoding     ( encodeUtf8 )
import Data.Time              ( getCurrentTime )
import GHC.Generics           ( Generic )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text           as Text
import qualified Data.Text.IO        as TextIO
import qualified Data.ByteString     as BS
import qualified Network.HTTP.Client as HTTP

-- TYPES AND INSTANCES ---------------------------------------------------------

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

instance Has Lock            Env where getter = envLock
instance Has (Logger App)    Env where getter = envLogger
instance Has (Requester App) Env where getter = envRequester
instance Has Token           Env where getter = envToken
instance Has Group           Env where getter = envGroup

newtype App a = App { unApp :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime App where
  getTime = liftIO getCurrentTime

instance MonadRequester App where
  requester manager req = liftIO $ try $ HTTP.httpLbs req manager

data Response a
  = Succes a
  | Error ErrorResponse
  deriving Show

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
    Succes <$> o .: "response"

data ErrorResponse = ErrorResponse
  { err :: Text } deriving (Generic, Show)

instance Aeson.FromJSON ErrorResponse where
  parseJSON = Aeson.parseJsonDrop

data GetLongPollServer = GetLongPollServer Token Group

instance ToRequest GetLongPollServer where
  toRequest (GetLongPollServer t g)
    = HTTP.urlEncodedBody (defaultBody t g)
    $ defaultRequest { HTTP.path = "/method/groups.getLongPollServer" }

data GetServer = GetServer
  { gsKey :: Text
  , gsTs  :: Text
  , gsServer :: Text
  } deriving (Generic, Show)

instance Aeson.FromJSON GetServer where
  parseJSON = Aeson.parseJsonDrop

instance ToRequest GetServer where
  toRequest GetServer {..} = mkBody $ defaultRequest
    { HTTP.path = snd splitUp
    , HTTP.host = fst splitUp
    }
    where
      splitUp = bimap encodeUtf8 encodeUtf8
              $ Text.span (/='/')
              $ fromMaybe gsServer
              $ Text.stripPrefix "https://" gsServer

      mkBody  = HTTP.urlEncodedBody
              [ ("act" , "a_check")
              , ("key" , encodeUtf8 gsKey)
              , ("wait", "25")
              , ("ts"  , encodeUtf8 gsTs)
              , ("mode", "2")
              ]

-- FUNCTIONS -------------------------------------------------------------------

app :: App ()
app = do
  r <- getLongPollServer >>= requestAndDecode @(Response GetServer)
  case r of
    Result (Succes v) -> do
      logDebug $ show v
      d <- requestAndDecode @(Response GetServer) v
      case d of
        Result v -> logDebug $ show v
        DecodeError e v -> logError e >> logDebug v
        RequestError e -> logError e
    Result x -> logError $ show x
    DecodeError e v -> logError e >> logDebug v
    RequestError e -> logError e

mkApp :: Config -> Logger.Config -> Lock -> HTTP.Manager -> Env
mkApp Config {..} cLogger lock = Env cToken cGroup logger lock . mkRequester
  where logger = mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getLongPollServer :: (Has Token r, Has Group r, MonadReader r m)
                  => m GetLongPollServer
getLongPollServer = GetLongPollServer
  <$> asks getter
  <*> asks getter

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }

defaultBody :: Token -> Group -> [(BS.ByteString, BS.ByteString)]
defaultBody t g =
  [ ("access_token", encodeUtf8 $ unToken t)
  , ("group_id"    , encodeUtf8 $ unGroup g)
  , ("v"           , "5.122")
  ]
