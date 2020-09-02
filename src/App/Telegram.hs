{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module App.Telegram ( Config, mkApp, runApp ) where

-- IMPORTS --------------------------------------------------------------------

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal                        ( Lock, Has (..) )

import qualified Infrastructure.Logger as Logger

import Control.Applicative    ( (<|>) )
import Control.Exception      ( try )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, asks, runReaderT )
import Data.Aeson.Extended    ( (.:) )
import Data.Text.Encoding     ( encodeUtf8 )
import Data.Text.Extended     ( Text )
import Data.Time              ( getCurrentTime )
import GHC.Generics           ( Generic )

import qualified Data.Aeson.Extended          as Aeson
import qualified Data.Text.Extended           as Text
import qualified Data.Text.IO                 as TextIO
import qualified Network.HTTP.Client.Extended as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

newtype Token = Token { unToken :: Text }

newtype Config = Config
  { cToken :: Token
  }

instance Loggable Config where toLog _ = "It's a trap"
instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Telegram.Config" $ \o -> Config
    <$> (Token <$> o .: "token")

data Env = Env
  { envToken     :: Token
  , envLogger    :: Logger App
  , envLock      :: Lock
  , envRequester :: Requester App
  }

instance Has Lock            Env where getter = envLock
instance Has (Logger App)    Env where getter = envLogger
instance Has (Requester App) Env where getter = envRequester
instance Has Token           Env where getter = envToken

newtype App a = App { unApp :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance Logger.MonadTime App where
  getTime = liftIO getCurrentTime

instance MonadRequester App where
  requester manager req = liftIO $ try $ HTTP.httpLbs req manager

data Response a
  = Succes a
  | Error Integer Text

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
        Succes <$> o .: "result"
    <|> Error  <$> o .: "error_code"
               <*> o .: "description"

instance Loggable a => Loggable (Response a) where
  toLog (Succes x) = toLog x

  toLog (Error code description)
    = "An error occurred as a result of the request\n\
    \ | Error Code: "        <> Text.showt code <> "\n\
    \ | Error Message: "     <> description

data GetUpdates = GetUpdates (Maybe Integer) Token

instance ToRequest GetUpdates where
  toRequest (GetUpdates Nothing t) = defaultRequest
    { HTTP.path = "/bot" <> (encodeUtf8 . unToken) t <> "/getUpdates"
    , HTTP.method = "GET"
    }

  toRequest (GetUpdates (Just n) t)
    = HTTP.urlEncodedBody mkBody $ defaultRequest
    { HTTP.path = "/bot" <> (encodeUtf8 . unToken) t <> "/getUpdates" }
    where mkBody = [ ("offset" , encodeUtf8 $ Text.pack $ show $ n + 1)
                   , ("timeout", "25")
                   ]

-- FUNCTIONS ---------------------------------------------------------------

app :: App ()
app = do
  t <- asks getter
  r <- requestAndDecode @(Response Config) $ GetUpdates Nothing t
  case r of
    Result (Succes v) -> logDebug ("successful" :: Text)
    err -> logError err

mkApp :: Config -> Logger.Config -> Lock -> HTTP.Manager -> Env
mkApp Config {..} cLogger lock = Env cToken logger lock . mkRequester
  where logger = mkLogger cLogger "Telegram"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host = "api.telegram.org"
  , HTTP.method = "POST"
  }
