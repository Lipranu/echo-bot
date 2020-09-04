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
import Data.Text.Extended     ( Text )
import Data.Maybe             ( fromMaybe )
import Data.Text.Encoding     ( encodeUtf8 )
import Data.Time              ( getCurrentTime )
import Data.Typeable          ( Typeable, typeOf )
import Data.Foldable          ( traverse_ )
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

instance Typeable a => Loggable (Response a) where
  toLog (Success x) =
    "Successfully received response of type: " <> Text.showt (typeOf x)
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
  { guKey  :: Text
  , guTs   :: Text
  , guPath :: Text
  , guHost :: Text
  }

instance Aeson.FromJSON GetUpdates where
  parseJSON = Aeson.withObject "App.Vk.GetUpdates" $ \o -> do
    guKey    <- o .: "key"
    guTs     <- o .: "ts"
    guServer <- o .: "server"
    let (guHost,guPath) = Text.span (/='/')
                        $ fromMaybe guServer
                        $ Text.stripPrefix "https://" guServer
    return GetUpdates {..}

instance MonadReader r m => ToRequest m r GetUpdates where
  toRequest GetUpdates {..} = return $ mkBody $ defaultRequest
    { HTTP.path = encodeUtf8 guPath
    , HTTP.host = encodeUtf8 guHost
    }
    where mkBody = HTTP.urlEncodedBody
                 [ ("act" , "a_check")
                 , ("key" , encodeUtf8 guKey)
                 , ("wait", "25")
                 , ("ts"  , encodeUtf8 guTs)
                 , ("mode", "2")
                 ]

instance Loggable GetUpdates where
  toLog _ = "Requesting updates from long poll server"

data Updates
  = Updates [Update] Text
  | OutOfDate Text
  | KeyExpired
  | DataLost

instance Aeson.FromJSON Updates where
  parseJSON = Aeson.withObject "App.Vk.Updates" $ \o -> do
    result <- o .:? "updates"
    case result of
      Just updates -> do
        ts <- o .: "ts"
        return $ Updates updates ts
      Nothing -> do
        code <- o .: "failed"
        case code :: Integer of
          1 -> OutOfDate <$> o .: "ts"
          2 -> return KeyExpired
          3 -> return DataLost
          e -> fail $ "App.Vk.Updates: Unknown error key: " <> show e

instance Loggable Updates where
  toLog (Updates upds ts) = "Resived updates:\n\
    \ | Amount: " <> (Text.showt . length) upds <> "\n\
    \ | New timestamp: " <> ts
  toLog (OutOfDate ts) =
    "Event history is outdated or partially lost. \
    \Performing new request for updates with timestamp: " <> ts
  toLog KeyExpired = "Key expired. Performing request for new key"
  toLog DataLost   = "Information lost. Performing request for new key"

data Update
  = NewMessage Message
  | NotSupported Text

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject "App.Vk.Update" $ \o -> do
    t <- o .: "type"
    case t of
      "message_new" -> NewMessage <$> (o .: "object" >>= (.: "message"))
      _ -> return $ NotSupported t

instance Loggable Update where
  toLog (NewMessage m) = toLog m
  toLog (NotSupported t) = "Not supprted update of type: " <> t

data Message = Message
  { mId   :: Integer
  , mText :: Text
  } deriving (Generic)

instance Aeson.FromJSON Message where parseJSON = Aeson.parseJsonDrop

instance Loggable Message where
  toLog Message {..} = "New message recived:\n\
    \ | Id: " <> Text.showt mId <> "\n\
    \ | Text: " <> mText

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
  logInfo GetLongPollServer
  result <- requestAndDecode GetLongPollServer
  case result of
    Result (Success gu) -> logDebug result >> getUpdates gu
    error -> logError error >> logError ("Application shut down" :: Text)

getUpdates :: GetUpdates -> App ()
getUpdates gu = do
  logDebug gu
  result <- requestAndDecode gu
  case result of
    Result (Updates upd ts) -> do
      logDebug result
      proccessUpdates upd
      getUpdates gu { guTs = ts }
    Result (OutOfDate ts) -> do
      logWarning result
      getUpdates gu { guTs = ts }
    Result v -> logWarning v >> getLongPollServer
    error -> logError error >> logError ("Application shut down" :: Text)

proccessUpdates :: [Update] -> App ()
proccessUpdates xs = traverse_ logDebug xs

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }

defaultBody :: Token -> Group -> [(BS.ByteString, BS.ByteString)]
defaultBody token group =
  [ ("access_token", encodeUtf8 $ unToken token)
  , ("group_id"    , encodeUtf8 $ unGroup group)
  , ("v"           , "5.122")
  ]
