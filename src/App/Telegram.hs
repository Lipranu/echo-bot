{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Telegram ( Config, mkApp, runApp ) where

-- IMPORTS --------------------------------------------------------------------

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal

import qualified Infrastructure.Logger as Logger

import Control.Applicative         ( (<|>) )
import Control.Exception           ( try )
import Control.Monad               ( foldM )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader        ( ReaderT, MonadReader, runReaderT )
import Data.Aeson.Extended         ( (.:) )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8 )
import Data.Text.Extended          ( Text )
import Data.Time                   ( getCurrentTime )

import qualified Data.Aeson.Extended          as Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text.Extended           as Text
import qualified Data.Text.IO                 as TextIO
import qualified Network.HTTP.Client.Extended as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

-- Config and Env ----------------------------------------------------------

newtype Token = Token { unToken :: Text }

newtype Config = Config { cToken :: Token }

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

-- App ---------------------------------------------------------------------

newtype App a = App { unApp :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance Logger.MonadTime App where
  getTime = liftIO getCurrentTime

instance MonadRequester App where
  requester manager req = liftIO $ try $ HTTP.httpLbs req manager

-- Response ----------------------------------------------------------------

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
    \ | Error Description: " <> description

-- GetUpdates --------------------------------------------------------------

newtype GetUpdates = GetUpdates (Maybe Integer)

instance (Has Token r, MonadReader r m) => ToRequest m r GetUpdates where
  toRequest (GetUpdates Nothing) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody defaultGetUpdatesBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates"
      , HTTP.method = "GET"
      }

  toRequest (GetUpdates (Just n)) = do
    token <- obtain
    return $
      HTTP.urlEncodedBody mkBody $
      defaultRequest
      { HTTP.path = defaultPath token <> "/getUpdates" }
    where mkBody = ("offset" , encodeShowUtf8 $ n + 1)
                 : defaultGetUpdatesBody

instance Loggable GetUpdates where
  toLog (GetUpdates Nothing)
    = "GetUpdates request without offset"
  toLog (GetUpdates (Just n))
    = "GetUpdates request with offset: " <> Text.showt (n + 1)

-- Update ------------------------------------------------------------------

newtype Update = Post Integer

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject "App.Vk.Update" $ \o -> Post
    <$> o .: "update_id"

instance Loggable [Update] where
  toLog v = "Updates resived: " <> Text.showt (length v)

instance Loggable Update where
  toLog (Post i) = "Proccess post with id: " <> Text.showt i

-- FUNCTIONS ---------------------------------------------------------------

app :: App ()
app = do
  logInfo ("Application getting started" :: Text)
  getUpdates $ GetUpdates Nothing

mkApp :: Config -> Logger.Config -> Lock -> HTTP.Manager -> Env
mkApp Config {..} cLogger lock = Env cToken logger lock . mkRequester
  where logger = mkLogger cLogger "Telegram"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getUpdates :: GetUpdates -> App ()
getUpdates gu = do
  logInfo gu
  result <- requestAndDecode gu
  case result of
    Result (Succes v) -> do
      logInfo v
      proccessUpdates v
    error -> do
      logError error
      logError ("Application shut down" :: Text)

proccessUpdates :: [Update] -> App ()
proccessUpdates xs = do
  x <- foldM proccessUpdate Nothing xs
  getUpdates $ GetUpdates x

proccessUpdate :: Maybe Integer -> Update -> App (Maybe Integer)
proccessUpdate current p@(Post id) = do
  logDebug p
  return (max current $ Just id)

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest
  { HTTP.host = "api.telegram.org"
  , HTTP.method = "POST"
  }

defaultPath :: Token -> BS.ByteString
defaultPath token = "/bot" <> encodeUtf8 (unToken token)

defaultGetUpdatesBody :: [(BS.ByteString, BS.ByteString)]
defaultGetUpdatesBody =
  let list :: [Text]
      list = ["message", "edited_channel_post", "callback_query"]
   in [ ("timeout", "25")
      , ("allowed_updates", LBS.toStrict $ Aeson.encode list)
      ]
