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
import Control.Exception      ( try )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT, MonadReader, runReaderT )
import Data.Aeson             ( (.:), (.:?) )
import Data.Foldable          ( traverse_ )
import Data.Maybe             ( fromMaybe )
import Data.Text.Encoding     ( encodeUtf8, decodeUtf8 )
import Data.Text.Extended     ( Text )
import Data.Time              ( getCurrentTime )
import Data.Typeable          ( Typeable, typeOf )
import GHC.Generics           ( Generic )
import System.Random          ( randomIO )

import qualified Data.Aeson.Extended          as Aeson
import qualified Data.Text.Extended           as Text
import qualified Data.Text.IO                 as TextIO
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Network.HTTP.Client.Extended as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

-- Config and Env ----------------------------------------------------------

newtype Token = Token { unToken :: Text }

newtype Group = Group { unGroup :: Text }

data Config = Config
  { cToken :: Token
  , cGroup :: Group
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Vk.Config" $ \o -> Config
    <$> (Token <$> o .: "token")
    <*> (Group <$> o .: "group_id")

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

-- App ---------------------------------------------------------------------

newtype App a = App { unApp :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadFail)

instance MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime App where
  getTime = liftIO getCurrentTime

instance MonadRequester App where
  requester manager req = liftIO $ try $ HTTP.httpLbs req manager

-- Response ----------------------------------------------------------------

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

-- ErrorResponse -----------------------------------------------------------

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

-- RequestParams -----------------------------------------------------------

data RequestParams = RequestParams
  { rpKey   :: Text
  , rpValue :: Text
  } deriving Generic

instance Aeson.FromJSON RequestParams where
  parseJSON = Aeson.parseJsonDrop

instance Loggable RequestParams where
  toLog RequestParams {..} = "\n | \t" <> rpKey <> ": " <> rpValue

-- GetLongPollServer -------------------------------------------------------

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

-- GetUpdates --------------------------------------------------------------

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

-- Updates -----------------------------------------------------------------

data Updates
  = Updates [Aeson.Value] Text
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

-- Update ------------------------------------------------------------------

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

-- Message -----------------------------------------------------------------

data Message = Message
  { mFromId      :: Integer
  , mPeerId      :: Integer
  , mText        :: Maybe Text
  , mLatitude    :: Maybe Double
  , mLongitude   :: Maybe Double
--  , mAttachments :: Maybe [Aeson.Object]
--  , mKeyboard    :: Maybe Aeson.Object
  } deriving (Generic)

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "App.Vk.Message" $ \o -> Message
    <$> o .:  "from_id"
    <*> o .:  "peer_id"
    <*> o .:? "text"
    <*> (coord o "latitude"  <|> pure Nothing)
    <*> (coord o "longitude" <|> pure Nothing)
    where coord o t = o .: "geo" >>= (.: "coordinates") >>= (.: t)

instance Loggable Message where
  toLog Message {..} = "New message recived:\n\
    \ | from_id: "     <> Text.showt mFromId      <> "\n\
    \ | peer_id: "     <> Text.showt mPeerId      <> "\n\
    \ | text: "        <> fromMaybe "" mText               --    <> "\n\
--    \ | attachments: " <> Text.showt mAttachments <> "\n\
--    \ | geo: "         <> Text.showt mGeo         <> "\n\
--    \ | keyboard: "    <> Text.showt mKeyboard

-- SendMessage -------------------------------------------------------------

data SendMessage = SendMessage
  { smPeerId :: Integer
  , smRandomId :: Int
  , smMessage  :: Maybe Text
  , smLatitude    :: Maybe Double
  , smLongitude  :: Maybe Double
  } deriving Generic

instance Aeson.ToJSON SendMessage where
  toJSON = Aeson.toJsonDrop

instance (Has Token r, Has Group r, MonadReader r m)
  => ToRequest m r SendMessage where
  toRequest SendMessage {..} = do
    token <- obtain
    group <- obtain
    return $ mkBody token group $ defaultRequest
      { HTTP.method = "POST"
      , HTTP.path   = "method/messages.send"
      }
    where mkBody token group = HTTP.urlEncodedBody
            $ addMaybeToBody ("message", encodeUtf8 <$> smMessage)
            $ addMaybeToBody ("lat", (encodeUtf8 . Text.showt) <$> smLatitude)
            $ addMaybeToBody ("long", (encodeUtf8 . Text.showt) <$> smLongitude)
            $ [ ("peer_id"  , encodeUtf8 $ Text.showt smPeerId)
              , ("random_id", encodeUtf8 $ Text.showt smRandomId)
              ] ++ defaultBody token group

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
      proccessUpdates $ parse <$> upd
      getUpdates gu { guTs = ts }
    Result (OutOfDate ts) -> do
      logWarning result
      getUpdates gu { guTs = ts }
    Result v -> logWarning v >> getLongPollServer
    error -> logError error >> logError ("Application shut down" :: Text)

proccessUpdates :: [Result Update] -> App ()
proccessUpdates = traverse_ handleUpdateErrors

handleUpdateErrors :: Result Update -> App ()
handleUpdateErrors (Result (NewMessage m))
  = logDebug m
 >> proccessNewMessage m
handleUpdateErrors error = logWarning error

proccessNewMessage :: Message -> App ()
proccessNewMessage message = do
  id <- liftIO $ randomIO :: App Int
  result <- request $ convertMessage id message
  case result of
    Result v -> logDebug $ decodeUtf8 $ LBS.toStrict v
    RequestError error -> logWarning error

convertMessage :: Int -> Message -> SendMessage
convertMessage id Message {..} = SendMessage
  { smPeerId    = mPeerId
  , smMessage   = mText
  , smRandomId  = id
  , smLatitude  = mLatitude
  , smLongitude = mLongitude
  }

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }

defaultBody :: Token -> Group -> [(BS.ByteString, BS.ByteString)]
defaultBody token group =
  [ ("access_token", encodeUtf8 $ unToken token)
  , ("group_id"    , encodeUtf8 $ unGroup group)
  , ("v"           , "5.122")
  ]

addMaybeToBody :: (BS.ByteString, Maybe BS.ByteString)
               -> [(BS.ByteString, BS.ByteString)]
               -> [(BS.ByteString, BS.ByteString)]
addMaybeToBody (_   , Nothing)    body = body
addMaybeToBody (name, Just value) body = (name, value) : body
