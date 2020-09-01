{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ExplicitForAll           #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS --------------------------------------------------------------------

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal

import qualified Infrastructure.Logger as Logger

import Control.Applicative    ( (<|>) )
import Control.Monad          ( (>=>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT, asks )
import Control.Exception      ( try )
import Data.Aeson             ( (.:), (.:?) )
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

-- TYPES AND INSTANCES --------------------------------------------------------

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
  = Succes a
  | Error ErrorResponse
  deriving Show

instance Aeson.FromJSON a => Aeson.FromJSON (Response a) where
  parseJSON = Aeson.withObject "App.Vk.Response" $ \o ->
        Succes <$> o .: "response"
    <|> Error  <$> o .: "error"

data ErrorResponse = ErrorResponse
  { eErrorCode     :: Integer
  , eErrorMsg      :: Text
  , eRequestParams :: [RequestParams]
  } deriving (Generic, Show)

instance Aeson.FromJSON ErrorResponse where
  parseJSON = Aeson.parseJsonDrop

instance Loggable ErrorResponse where
  toLog ErrorResponse {..}
    = "An error occurred as a result of the request\n"
   <> " | Error Code: "        <> code
   <> " | Error Message: "     <> message
   <> " | Request Parameters:" <> params
    where code    = (Text.pack . show) eErrorCode <> "\n"
          message = eErrorMsg <> "\n"
          params  = foldr (<>) "" $ fmap toLog eRequestParams

data RequestParams = RequestParams
  { rpKey   :: Text
  , rpValue :: Text
  } deriving (Generic, Show)

instance Aeson.FromJSON RequestParams where
  parseJSON = Aeson.parseJsonDrop

instance Loggable RequestParams where
  toLog RequestParams {..} = "\n | \t" <> rpKey <> ": " <> rpValue

data GetLongPollServer = GetLongPollServer Token Group

instance ToRequest GetLongPollServer where
  toRequest (GetLongPollServer t g)
    = HTTP.urlEncodedBody (defaultBody t g)
    $ defaultRequest { HTTP.path = "/method/groups.getLongPollServer" }

data GetUpdates = GetUpdates
  { gsKey    :: Text
  , gsTs     :: Text
  , gsServer :: Text
  } deriving (Generic, Show)

instance Aeson.FromJSON GetUpdates where
  parseJSON = Aeson.parseJsonDrop

instance ToRequest GetUpdates where
  toRequest GetUpdates {..} = mkBody $ defaultRequest
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

data Updates
  = Updates [Aeson.Object] Text
  | OutOfDateOrLost Text
  | KeyExpired
  | DataLost
  deriving Show

instance Aeson.FromJSON Updates where
  parseJSON = Aeson.withObject "App.Vk.Updates" $ \o -> do
    v <- o .:? "updates"
    case v of
      Just r -> Updates r <$> o .: "ts"
      Nothing -> do
        i <- o .: "failed"
        case i :: Integer of
          1 -> OutOfDateOrLost <$> o .: "ts"
          2 -> return KeyExpired
          3 -> return DataLost
          e -> fail $ "App.Vk.Updates: Unknown error key: " <> show e

-- FUNCTIONS -----------------------------------------------------------------

app :: App ()
app = do
  s <- requestLongPollServer
  u <- handleErrors @Updates s
  updatesHandler u s

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

handleErrors :: forall b a . (ToRequest a, Show b, Aeson.FromJSON b) => a -> App b
handleErrors req = do
  v <- requestAndDecode req
  case v of
    Result v -> do
      logDebug $ show v
      return v
    DecodeError e v  -> logError e >> logDebug v >> fail ""
    RequestError e   -> logError e >> fail ""

handleResult :: forall a . Show a => Response a -> App a
handleResult (Error x)  = logError x >> fail ""
handleResult (Succes v) = logInfo ("Successful request" :: Text)
                       >> logDebug (show v)
                       >> return v

handler :: forall b a . (ToRequest a, Show b, Aeson.FromJSON b) => a -> App b
handler = handleErrors >=> handleResult

requestLongPollServer :: App GetUpdates
requestLongPollServer = getLongPollServer >>= handler

updatesHandler :: Updates -> GetUpdates -> App () --GetUpdates
updatesHandler DataLost             _ = do
  logWarning ("Information is lost, performing request for new key and ts" :: Text)
  server  <- requestLongPollServer
  updates <- handleErrors @Updates server
  updatesHandler updates server
updatesHandler KeyExpired           _ = do
  logInfo ("The key has expired, performing request for new key" :: Text)
  fail "gsg"
updatesHandler (OutOfDateOrLost ts) _ = do
  logWarning ("The event history is out of date or has been partially lost" :: Text)
  fail "gsg"
updatesHandler (Updates ts upd)     _ = do
  logWarning ("Information is lost, performing request for new key and ts" :: Text)
  fail "gsg"

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }

defaultBody :: Token -> Group -> [(BS.ByteString, BS.ByteString)]
defaultBody t g =
  [ ("access_token", encodeUtf8 $ unToken t)
  , ("group_id"    , encodeUtf8 $ unGroup g)
  , ("v"           , "5.122")
  ]
