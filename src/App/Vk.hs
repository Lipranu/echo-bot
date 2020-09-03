{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal

import qualified Infrastructure.Logger as Logger

import Control.Applicative    ( (<|>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT )
import Control.Exception      ( try )
import Data.Aeson             ( (.:), (.:?) )
import Data.Bifunctor         ( bimap )
import Data.Text.Extended     ( Text )
import Data.Maybe             ( fromMaybe )
import Data.Text.Encoding     ( encodeUtf8 )
import Data.Time              ( getCurrentTime )
import GHC.Generics           ( Generic )

import qualified Data.Aeson.Extended          as Aeson
import qualified Data.Text.Extended           as Text
import qualified Data.Text.IO                 as TextIO
import qualified Data.ByteString              as BS
import qualified Network.HTTP.Client.Extended as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

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
  (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadFail)

instance MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime App where
  getTime = liftIO getCurrentTime

instance MonadRequester App where
  requester manager req = liftIO $ try $ HTTP.httpLbs req manager

data Response a
  = Success a
  | Error ErrorResponse

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
        Success <$> o .: "response"
    <|> Error   <$> o .: "error"

instance Loggable a => Loggable (Response a) where
  toLog (Success x) = toLog x
  toLog (Error   x) = toLog x

data ErrorResponse = ErrorResponse
  { eErrorCode     :: Integer
  , eErrorMsg      :: Text
  , eRequestParams :: [RequestParams]
  } deriving Generic

instance Aeson.FromJSON ErrorResponse where
  parseJSON = Aeson.parseJsonDrop

instance Loggable ErrorResponse where
  toLog ErrorResponse {..}
    = "An error occurred as a result of the request\n\
    \ | Error Code: "        <> Text.showt eErrorCode <> "\n\
    \ | Error Message: "     <> eErrorMsg             <> "\n\
    \ | Request Parameters:" <> params
    where params  = foldr (<>) "" $ fmap toLog eRequestParams

data RequestParams = RequestParams
  { rpKey   :: Text
  , rpValue :: Text
  } deriving Generic

instance Aeson.FromJSON RequestParams where
  parseJSON = Aeson.parseJsonDrop

instance Loggable RequestParams where
  toLog RequestParams {..} = "\n | \t" <> rpKey <> ": " <> rpValue

data GetLongPollServer = GetLongPollServer

instance (Has Token r, Has Group r, MonadReader r m) =>
  ToRequest m r GetLongPollServer where
  toRequest GetLongPollServer = do
    token <- obtain
    group <- obtain
    return $ HTTP.urlEncodedBody (defaultBody token group)
           $ defaultRequest
           { HTTP.path = "/method/groups.getLongPollServer" }

instance Loggable GetLongPollServer where
  toLog _ = "Requesting long poll server"

data GetUpdates = GetUpdates
  { guKey    :: Text
  , guTs     :: Text
  , guServer :: Text
  } deriving Generic

instance Aeson.FromJSON GetUpdates where
  parseJSON = Aeson.parseJsonDrop

instance MonadReader r m => ToRequest m r GetUpdates where
  toRequest GetUpdates {..} = return $ mkBody $ defaultRequest
    { HTTP.path = snd splitUp
    , HTTP.host = fst splitUp
    }
    where
      splitUp = bimap encodeUtf8 encodeUtf8
              $ Text.span (/='/')
              $ fromMaybe guServer
              $ Text.stripPrefix "https://" guServer

      mkBody  = HTTP.urlEncodedBody
              [ ("act" , "a_check")
              , ("key" , encodeUtf8 guKey)
              , ("wait", "25")
              , ("ts"  , encodeUtf8 guTs)
              , ("mode", "2")
              ]

instance Loggable GetUpdates where
  toLog _ = "Requesting updates from long poll server"

data Updates
  = Updates [Aeson.Object] Text
  | OutOfDate Text
  | KeyExpired
  | DataLost

instance Aeson.FromJSON Updates where
  parseJSON = Aeson.withObject "App.Vk.Updates" $ \o -> do
    v <- o .:? "updates"
    case v of
      Just r -> Updates r <$> o .: "ts"
      Nothing -> do
        i <- o .: "failed"
        case i :: Integer of
          1 -> OutOfDate <$> o .: "ts"
          2 -> return KeyExpired
          3 -> return DataLost
          e -> fail $ "App.Vk.Updates: Unknown error key: " <> show e

instance Loggable Updates where
  toLog (Updates upds ts) = "Resived updates: "
    <> (Text.showt . length) upds
    <> " | New timestamp: "
    <> ts
  toLog (OutOfDate ts) =
    "Event history is outdated or partially lost. \
    \Performing new request for updates with timestamp: " <> ts
  toLog KeyExpired = "Key expired. Performing request for new key"
  toLog DataLost   = "Information lost. Performing request for new key"

-- FUNCTIONS ---------------------------------------------------------------

app :: App ()
app = do
  logInfo ("Application getting started" :: Text)
  getLongPollServer

mkApp :: Config -> Logger.Config -> Lock -> HTTP.Manager -> Env
mkApp Config {..} cLogger lock = Env cToken cGroup logger lock . mkRequester
  where logger = mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getLongPollServer :: App ()
getLongPollServer = do
  let lp = GetLongPollServer
  logInfo lp
  result <- requestAndDecode lp
  case result of
    Result (Success gu) -> do
      logInfo gu
      getUpdates gu
    error -> do
      logError error
      logError ("Application shut down" :: Text)

getUpdates :: GetUpdates -> App ()
getUpdates gu = do
  logInfo gu
  result <- requestAndDecode gu
  case result of
    Result (Updates upd ts) -> do
      logInfo result
      proccessUpdates upd
      getUpdates gu { guTs = ts }
    Result (OutOfDate ts) -> do
      logWarning result
      getUpdates gu { guTs = ts }
    Result v -> do
      logWarning v
      getLongPollServer
    error -> do
      logError error
      logError ("Application shut down" :: Text)

proccessUpdates :: [Aeson.Object] -> App ()
proccessUpdates xs = logInfo ("Proccessing updates" :: Text)

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }

defaultBody :: Token -> Group -> [(BS.ByteString, BS.ByteString)]
defaultBody token group =
  [ ("access_token", encodeUtf8 $ unToken token)
  , ("group_id"    , encodeUtf8 $ unGroup group)
  , ("v"           , "5.122")
  ]
