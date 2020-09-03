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

instance Loggable a => Loggable (Response a) where
  toLog (Succes x) = toLog x
  toLog (Error x)  = toLog x

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

data GetLongPollServer = GetLongPollServer

instance (Has Token r, Has Group r, MonadReader r m) =>
  ToRequest m r GetLongPollServer where
  toRequest GetLongPollServer = do
    token <- obtain
    group <- obtain
    return $ HTTP.urlEncodedBody (defaultBody token group)
           $ defaultRequest
           { HTTP.path = "/method/groups.getLongPollServer" }

data GetUpdates = GetUpdates
  { guKey    :: Text
  , guTs     :: Text
  , guServer :: Text
  } deriving (Generic, Show)

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
  toLog _ = "getUpdates"

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

instance Loggable Updates where
  toLog _ = "updates"

-- FUNCTIONS -----------------------------------------------------------------

app :: App ()
app = getLongPollServer

mkApp :: Config -> Logger.Config -> Lock -> HTTP.Manager -> Env
mkApp Config {..} cLogger lock = Env cToken cGroup logger lock . mkRequester
  where logger = mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getLongPollServer :: App ()
getLongPollServer = do
  result <- requestAndDecode GetLongPollServer
  case result of
    Result (Succes v) -> logDebug v >> getUpdates v
    err               -> logError err

getUpdates :: GetUpdates -> App ()
getUpdates gu = do
  result <- requestAndDecode gu
  case result of
    Result (OutOfDateOrLost ts)
      -> logWarning result
      >> getUpdates gu { guTs = ts }
    Result (Updates upd ts)
      -> logInfo result
      >> proccessUpdates upd
      >> getUpdates gu { guTs = ts }
    Result v
      -> logWarning result
      >> getLongPollServer
    err -> logError err

proccessUpdates :: [Aeson.Object] -> App ()
proccessUpdates upds = logInfo $ Text.showt $ length upds

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }

defaultBody :: Token -> Group -> [(BS.ByteString, BS.ByteString)]
defaultBody t g =
  [ ("access_token", encodeUtf8 $ unToken t)
  , ("group_id"    , encodeUtf8 $ unGroup g)
  , ("v"           , "5.122")
  ]
