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

import Control.Applicative                   ( (<|>) )
import Control.Exception                     ( try )
import Control.Monad                         ( foldM )
import Control.Monad.IO.Class                ( MonadIO, liftIO )
import Control.Monad.Reader                  ( ReaderT, MonadReader
                                             , runReaderT )
import Data.Aeson                            ( (.:), (.:?) )
import Data.Foldable                         ( traverse_ )
import Data.Maybe                            ( fromMaybe )
import Data.Text.Encoding.Extended           ( encodeUtf8, encodeShowUtf8
                                             , decodeUtf8 )
import Data.Text.Extended                    ( Text )
import Data.Time                             ( getCurrentTime )
import Data.Typeable                         ( Typeable, typeOf )
import GHC.Generics                          ( Generic )
import Network.HTTP.Client.MultipartFormData ( formDataBody, partFileSource )
import System.Random                         ( randomIO )

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
  toRequest GetLongPollServer = HTTP.urlEncodedBody
    <$> defaultBody
    <*> pure defaultRequest
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

-- SendMessage -------------------------------------------------------------

data SendMessage = SendMessage
  { smPeerId :: Integer
  , smRandomId    :: Int
  , smMessage     :: Maybe Text
  , smLatitude    :: Maybe Double
  , smLongitude   :: Maybe Double
  , smAttachments :: Maybe Text
  } deriving Generic

instance Aeson.ToJSON SendMessage where
  toJSON = Aeson.toJsonDrop

instance (Has Token r, Has Group r, MonadReader r m)
  => ToRequest m r SendMessage where
  toRequest SendMessage {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody
      (mergeBodies mBody $ body <> df)
      defaultRequest
        { HTTP.method = "POST"
        , HTTP.path   = "method/messages.send"
        }
    where body  = [ ("peer_id"  , encodeShowUtf8 smPeerId)
                  , ("random_id", encodeShowUtf8 smRandomId)
                  ]
          mBody = [ ("message"   , encodeUtf8     <$> smMessage)
                  , ("attachment", encodeUtf8     <$> smAttachments)
                  , ("lat"       , encodeShowUtf8 <$> smLatitude)
                  , ("long"      , encodeShowUtf8 <$> smLongitude)
                  ]

-- Attachment --------------------------------------------------------------

data Attachment = Attachment
  { aType      :: Text
  , aMediaId   :: Integer
  , aOwnerId   :: Integer
  , aAccessKey :: Maybe Text
  , aUrl       :: Maybe Text
  }

instance Aeson.FromJSON Attachment where
  parseJSON = Aeson.withObject "App.Vk.Attachment" $ \o -> do
    aType      <- o .: "type"
    aMediaId   <- o .: aType >>= (.:  "id")
    aOwnerId   <- o .: aType >>= (.:  "owner_id")
    aAccessKey <- o .: aType >>= (.:? "access_key")
    aUrl       <- o .: aType >>= (.:? "url")
    return Attachment {..}

instance Loggable Attachment where
  toLog Attachment {..} = "Proccessing attachment:\n\
    \ | Type: "     <> aType               <> "\n\
    \ | Media Id: " <> Text.showt aMediaId <> "\n\
    \ | Owner Id: " <> Text.showt aOwnerId <> key
    where key = case aAccessKey of
            Just v  -> "\n | Access Key: " <> v
            Nothing -> mempty

-- GetFile -----------------------------------------------------------------

newtype GetFile = GetFile Text

instance MonadReader r m => ToRequest m r GetFile where
  toRequest (GetFile url) = return $ HTTP.parseRequest_ $ Text.unpack url

-- GetUploadServer ---------------------------------------------------------

data GetUploadServer = GetUploadServer
  { gusType :: Text
  , gusPeerId :: Integer
  }

instance (Has Token r, Has Group r, MonadReader r m)
  => ToRequest m r GetUploadServer where
  toRequest GetUploadServer {..} = do
    df <- defaultBody
    return $ HTTP.urlEncodedBody (body <> df)
           $ defaultRequest { HTTP.method = "GET"
                            , HTTP.path   = "/method/docs.getMessagesUploadServer"
                            }
    where body = [ ("type"   , encodeUtf8 gusType)
                 , ("peer_id", encodeShowUtf8 gusPeerId)
                 ]

-- UploadServer ------------------------------------------------------------

newtype UploadServer = UploadServer Text

instance Aeson.FromJSON UploadServer where
  parseJSON = Aeson.withObject "App.Vk.UploadServer" $ \o ->
    UploadServer <$> o .: "upload_url"

-- UploadDocument ----------------------------------------------------------

data UploadDocument = UploadDocument
  { udFile :: FilePath --BS.ByteString
  , udUrl  :: Text
  }

instance (MonadReader r m, MonadIO m) => ToRequest m r UploadDocument where
  toRequest UploadDocument {..} =
    let req = HTTP.parseRequest_ $ Text.unpack udUrl
     in liftIO $ formDataBody [partFileSource "file" udFile] req
                                   --partBS "file" udFile] req

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
handleUpdateErrors (Result (NewMessage m)) = do
  logDebug m
  proccessNewMessage m
handleUpdateErrors error = logWarning error

proccessNewMessage :: Message -> App ()
proccessNewMessage message@Message {..} = do
  id          <- liftIO randomIO
  attachments <- proccessAttachments mPeerId mAttachments
  result      <- request $ convertMessage id attachments message
  case result of
    Result v -> logDebug $ decodeUtf8 $ LBS.toStrict v
    RequestError error -> logWarning error

resultAttachments :: [Result Attachment] -> App [Attachment]
resultAttachments = foldM func []
  where func xs (Result r) = logDebug r >> return (r : xs)
        func xs x = logWarning x >> return xs

convertAttachments :: [Attachment] -> Maybe Text
convertAttachments []     = Nothing
convertAttachments attach = Just $ Text.intercalate "," $ map convert attach
  where convert Attachment {..}
          =  aType
          <> Text.showt aOwnerId
          <> "_"
          <> Text.showt aMediaId
          <> case aAccessKey of
          Just v -> "_" <> v
          Nothing -> mempty

proccessAttachments :: Integer -> [Aeson.Value] -> App (Maybe Text)
proccessAttachments peerId rawAttachments = do
  attachments <- resultAttachments $ parse <$> rawAttachments
  traverse_ (uploadAttachments peerId) attachments
  return $ convertAttachments attachments

convertMessage :: Int -> Maybe Text -> Message -> SendMessage
convertMessage id attach Message {..} = SendMessage
  { smPeerId      = mPeerId
  , smMessage     = mText
  , smRandomId    = id
  , smLatitude    = mLatitude
  , smLongitude   = mLongitude
  , smAttachments = attach
  }

uploadAttachments :: Integer -> Attachment -> App ()
uploadAttachments peerId Attachment {..} = case aType of
  "doc" -> case aUrl of
    Nothing -> logWarning ("Empty url in doc file attachment" :: Text)
    Just url1 -> do
      file <- request $ GetFile url1
      uploadServer <- requestAndDecode $ GetUploadServer "doc" peerId
      case (file, uploadServer) of
        (Result r, Result (Success (UploadServer url2))) -> do
            liftIO $ BS.writeFile "lecture-1-sets.pdf" $ LBS.toStrict r
            req <- request $
              UploadDocument { udFile = "lecture-1-sets.pdf", udUrl = url2 }
            case req of
              Result res -> logDebug $ decodeUtf8 $ LBS.toStrict res
              RequestError err -> logWarning err
        (RequestError e1, e2) -> logWarning e1 >> logWarning e2
        (_, e2) -> logWarning e2
  _ -> return ()

defaultRequest :: HTTP.Request
defaultRequest = HTTP.defaultRequest { HTTP.host = "api.vk.com" }

defaultBody :: (Has Token r, Has Group r, MonadReader r m)
            => m [(BS.ByteString, BS.ByteString)]
defaultBody = do
  token <- obtain
  group <- obtain
  return
    [ ("access_token", encodeUtf8 $ unToken token)
    , ("group_id"    , encodeUtf8 $ unGroup group)
    , ("v"           , "5.122")
    ]

mergeBodies :: [(a, Maybe b)] -> [(a, b)] -> [(a, b)]
mergeBodies mBody body = foldr filterMaybe body mBody
  where filterMaybe (_, Nothing)       xs = xs
        filterMaybe (name, Just value) xs = (name, value) : xs
