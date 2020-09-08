{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Internal
import App.Vk.Requests

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal

import qualified Infrastructure.Logger as Logger

import Control.Applicative         ( (<|>) )
import Control.Exception           ( try )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader        ( ReaderT, MonadReader, runReaderT, lift )
import Control.Monad.State         ( StateT, execStateT, modify, gets )
import Data.Aeson                  ( (.:), (.:?) )
import Data.Foldable               ( traverse_ )
import Data.Maybe                  ( fromMaybe )
import Data.Text.Encoding.Extended ( decodeUtf8 )
import Data.Text.Extended          ( Text )
import Data.Time                   ( getCurrentTime )
import Data.Typeable               ( Typeable, typeOf )
import GHC.Generics                ( Generic )
import System.Random               ( randomIO )

import qualified Data.Aeson.Extended          as Aeson
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text.Extended           as Text
import qualified Data.Text.IO                 as TextIO
import qualified Network.HTTP.Client.Extended as HTTP

-- TYPES AND INSTANCES -----------------------------------------------------

-- Config and Env ----------------------------------------------------------

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
  (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadFail )

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
  , mAttachments :: [Aeson.Value]
--  , mKeyboard    :: Maybe Aeson.Object
  }

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "App.Vk.Message" $ \o -> Message
    <$> o .:  "from_id"
    <*> o .:  "peer_id"
    <*> o .:? "text"
    <*> (coord o "latitude"  <|> pure Nothing)
    <*> (coord o "longitude" <|> pure Nothing)
    <*> o .: "attachments"
    where coord o t = o .: "geo" >>= (.: "coordinates") >>= (.: t)

instance Loggable Message where
  toLog Message {..} = "New message recived:\n\
    \ | from_id: "     <> Text.showt mFromId      <> "\n\
    \ | peer_id: "     <> Text.showt mPeerId      <> "\n\
    \ | text: "        <> fromMaybe "" mText               --    <> "\n\
--    \ | attachments: " <> Text.showt mAttachments <> "\n\
--    \ | geo: "         <> Text.showt mGeo         <> "\n\
--    \ | keyboard: "    <> Text.showt mKeyboard

-- Attachment --------------------------------------------------------------

data Attachment
  = Attachment Text AttachmentBody
  | Document DocumentBody

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject "App.Vk.Attachment" $ \o -> do
    aType      <- o .: "type"
    case aType of
      "doc" -> Document         <$> (o .: aType >>= Aeson.parseJSON)
      _     -> Attachment aType <$> (o .: aType >>= Aeson.parseJSON)

instance Loggable Attachment where
  toLog _ = "PlaceHolder"--"Proccessing attachment:\n\
--    \ | Type: "     <> aType               <> "\n\
--    \ | Media Id: " <> Text.showt aMediaId <> "\n\
--    \ | Owner Id: " <> Text.showt aOwnerId <> key
--    where key = case aAccessKey of
--            Just v  -> "\n | Access Key: " <> v
--            Nothing -> mempty

-- AttachmentBody ----------------------------------------------------------

data AttachmentBody = AttachmentBody
  { aId        :: Integer
  , aOwnerId   :: Integer
  , aAccessKey :: Maybe Text
  } deriving Generic

instance Aeson.FromJSON AttachmentBody where
  parseJSON = Aeson.parseJsonDrop

-- AttachmentBody ----------------------------------------------------------

data DocumentBody = DocumentBody
  { dUrl    :: Text
  , dTitle  :: Text
  } deriving Generic

instance Aeson.FromJSON DocumentBody where
  parseJSON = Aeson.parseJsonDrop

-- UploadServer ------------------------------------------------------------

newtype UploadServer = UploadServer Text

instance Aeson.FromJSON UploadServer where
  parseJSON = Aeson.withObject "App.Vk.UploadServer" $ \o ->
    UploadServer <$> o .: "upload_url"

-- FileUploaded ------------------------------------------------------------

newtype FileUploaded = FileUploaded Text

instance Aeson.FromJSON FileUploaded where
  parseJSON = Aeson.withObject "App.Vk.FileUploaded" $ \o ->
    FileUploaded <$> o .: "file"

instance Loggable FileUploaded where
  toLog _ = "Uploaded file placeholder"

-- FileSaved ---------------------------------------------------------------

data FileSaved = FileSaved
  { fsType :: Text
  , fsMediaId :: Integer
  , fsOwnerId :: Integer
  }

instance Aeson.FromJSON FileSaved where
  parseJSON = Aeson.withObject "App.Vk.FileSaved" $ \o -> do
    fsType    <- o .: "type"
    fsMediaId <- o .: fsType >>= (.: "id")
    fsOwnerId <- o .: fsType >>= (.: "owner_id")
    return FileSaved {..}

instance Loggable FileSaved where
  toLog _ = "FileSaved placeholder"

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
      processUpdates $ parse <$> upd
      getUpdates gu { guTs = ts }
    Result (OutOfDate ts) -> do
      logWarning result
      getUpdates gu { guTs = ts }
    Result v -> logWarning v >> getLongPollServer
    error -> logError error >> logError ("Application shut down" :: Text)

processUpdates :: [Result Update] -> App ()
processUpdates = traverse_ handle
  where handle (Result (NewMessage m)) = do
          logDebug m
          processNewMessage m
        handle error = logWarning error

processNewMessage :: Message -> App ()
processNewMessage message = do
  randomId <- liftIO randomIO
  let attach = parse <$> mAttachments message
      sendm  = convertMessage randomId message
  result   <- request =<< execStateT (processAttachments attach) sendm
  case result of
    Result v -> logDebug $ decodeUtf8 $ LBS.toStrict v
    RequestError error -> logWarning error

convertMessage :: Int -> Message -> SendMessage
convertMessage randomId Message {..} = SendMessage
  { smPeerId      = mPeerId
  , smMessage     = mText
  , smRandomId    = randomId
  , smLatitude    = mLatitude
  , smLongitude   = mLongitude
  , smAttachments = []
  }

processAttachments :: [Result Attachment] -> StateT SendMessage App ()
processAttachments = traverse_ handle
  where handle (Result x) = lift (logDebug x) >> convertAttachment x
        handle error      = lift $ logWarning error

convertAttachment :: Attachment -> StateT SendMessage App ()
convertAttachment (Attachment aType body) = do
  modify $ \sm -> sm { smAttachments = convert body : smAttachments sm }
  where convert AttachmentBody {..}
          =  aType
          <> Text.showt aOwnerId
          <> "_"
          <> Text.showt aId
          <> case aAccessKey of
          Just v -> "_" <> v
          Nothing -> mempty
convertAttachment (Document (DocumentBody {..})) = do
  peerId <- gets smPeerId
  file <- lift $ request $ GetFile dUrl
  uploadServer <- lift $ requestAndDecode $ GetUploadServer "doc" peerId
  case (file, uploadServer) of
    (Result r, Result (Success (UploadServer url))) -> do
        request <- lift $ requestAndDecode $ UploadFile r url dTitle
        case request of
          Result x -> lift (logDebug x) >> saveFile x dTitle
          error -> lift $ logWarning error
    (Result r, error) -> lift $ logWarning error

saveFile :: FileUploaded -> Text -> StateT SendMessage App ()
saveFile (FileUploaded file) name = do
  lift $ logDebug ("in save document" :: Text)
  result <- lift $ requestAndDecode $ SaveFile file name
  case result of
    Result (Success x) -> lift (logDebug x) >> convertFile x
    error -> lift $ logWarning error

convertFile :: FileSaved -> StateT SendMessage App ()
convertFile FileSaved {..} = do
  modify $ \sm -> sm { smAttachments = convert : smAttachments sm }
  where convert = fsType
               <> Text.showt fsOwnerId
               <> "_"
               <> Text.showt fsMediaId
