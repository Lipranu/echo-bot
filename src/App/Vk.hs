{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Converters
import App.Vk.Internal
import App.Vk.Requests
import App.Vk.Responses

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal

import qualified Infrastructure.Logger as Logger

import Control.Exception           ( try )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader        ( ReaderT, MonadReader, runReaderT, lift )
import Control.Monad.State         ( StateT, execStateT, modify, gets )
import Data.Aeson                  ( (.:) )
import Data.Foldable               ( traverse_ )
import Data.Text.Encoding.Extended ( encodeUtf8, encodeShowUtf8, decodeUtf8 )
import Data.Text.Extended          ( Text )
import Data.Time                   ( getCurrentTime )
import System.Random               ( randomIO )
import Network.HTTP.Client         ( Manager, httpLbs )

import qualified Data.Aeson.Extended  as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Extended   as Text
import qualified Data.Text.IO         as TextIO

-- TYPES AND INSTANCES -----------------------------------------------------

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

newtype App a = App { unApp :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadFail )

instance MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime App where
  getTime = liftIO getCurrentTime

instance MonadRequester App where
  requester manager req = liftIO $ try $ httpLbs req manager

-- FUNCTIONS ---------------------------------------------------------------

app :: App ()
app = do
  logInfo ("Application getting started" :: Text)
  getLongPollServer

mkApp :: Config -> Logger.Config -> Lock -> Manager -> Env
mkApp Config {..} cLogger lock = Env cToken cGroup logger lock . mkRequester
  where logger = mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getLongPollServer :: App ()
getLongPollServer = do
  logInfo GetLongPollServer
  result <- requestAndDecode GetLongPollServer
  case result of
    Result (Success lps) -> do
      logDebug result
      getUpdates $ convert lps
    error -> logError error >> logError ("Application shut down" :: Text)

getUpdates :: GetUpdates -> App ()
getUpdates gu = do
  logDebug gu
  result <- requestAndDecode gu
  case result of
    Result (Updates upd ts) -> do
      logDebug result
      processUpdates $ parse <$> upd
      getUpdates gu { guTs = encodeUtf8 ts }
    Result (OutOfDate ts) -> do
      logWarning result
      getUpdates gu { guTs = encodeShowUtf8 ts }
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
convertAttachment (Attachment aType body) =
  modify $ \sm -> sm { smAttachments = convert body : smAttachments sm }
  where convert AttachmentBody {..}
          =  aType
          <> Text.showt aOwnerId
          <> "_"
          <> Text.showt aId
          <> case aAccessKey of
          Just v -> "_" <> v
          Nothing -> mempty
convertAttachment (Document DocumentBody {..}) = do
  peerId <- gets smPeerId
  file <- lift $ request $ GetFile dUrl
  uploadServer <- lift $ requestAndDecode $ GetUploadServer "doc" peerId
  case (file, uploadServer) of
    (Result r, Result (Success (UploadServer url))) -> do
        request <- lift $ requestAndDecode $ UploadFile r url dTitle
        case request of
          Result f@(FileUploaded x) -> lift (logDebug f) >> saveFile x dTitle
          error -> lift $ logWarning error
    (Result r, error) -> lift $ logWarning error

saveFile :: Text -> Text -> StateT SendMessage App ()
saveFile file name = do
  lift $ logDebug ("in save document" :: Text)
  result <- lift $ requestAndDecode $ SaveFile file name
  case result of
    Result (Success x) -> lift (logDebug x) >> convertFile x
    error -> lift $ logWarning error

convertFile :: FileSaved -> StateT SendMessage App ()
convertFile FileSaved {..} =
  modify $ \sm -> sm { smAttachments = convert : smAttachments sm }
  where convert = fsType
               <> Text.showt fsOwnerId
               <> "_"
               <> Text.showt fsMediaId
