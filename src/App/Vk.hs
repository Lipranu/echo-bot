{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds           #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Converters

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal

import qualified Infrastructure.Logger as Logger

import Control.Exception           ( try )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader        ( ReaderT, MonadReader, runReaderT, lift )
import Control.Monad.State         ( StateT, execStateT, gets )
import Data.Aeson                  ( (.:) )
import Data.Foldable               ( traverse_ )
import Data.Text.Encoding.Extended ( decodeUtf8 )
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
  logInfo start
  getLongPollServer

mkApp :: Config -> Logger.Config -> Lock -> Manager -> Env
mkApp Config {..} cLogger lock = Env cToken cGroup logger lock . mkRequester
  where logger = mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getLongPollServer :: App ()
getLongPollServer = do
  logInfo GetLongPollServer
  requestAndDecode GetLongPollServer >>= handle
  where handle :: Result (Response LongPollServer) -> App ()
        handle result@(Result (Success lps)) = do
          logDebug result
          logDebug lps
          getUpdates $ convert lps
        handle error = logError error >> logError shutdown

getUpdates :: GetUpdates -> App ()
getUpdates gu = do
  logInfo gu
  requestAndDecode gu >>= handle
  where handle :: Result Updates -> App ()
        handle r@(Result u@(Updates upd ts)) = do
          logDebug r
          logDebug u
          processUpdates $ parse <$> upd
          getUpdates gu { guTs = ts }
        handle r@(Result u@(OutOfDate ts)) = do
          logDebug r
          logWarning u
          getUpdates gu { guTs = Text.showt ts }
        handle r@(Result v) = do
          logDebug r
          logWarning v
          getLongPollServer
        handle error = do
          logError error
          logError shutdown

processUpdates :: [Result Update] -> App ()
processUpdates = traverse_ handle
  where handle r@(Result (NewMessage m)) = do
          logDebug r
          logDebug m
          processNewMessage m
        handle error = logWarning error

processNewMessage :: Message -> App ()
processNewMessage message = do
  randomId <- liftIO randomIO :: App Int
  let attach = parse <$> mAttachments message
      sendm  = convert message randomId
      state  = AttachmentsState [] Nothing $ mPeerId message
  handle =<< request . sendm =<< execStateT (processAttachments attach) state
  where handle :: Result LBS.ByteString -> App ()
        handle (Result v) = logDebug $ decodeUtf8 $ LBS.toStrict v
        handle (RequestError error) = logWarning error

processAttachments :: [Result Attachment] -> StateT AttachmentsState App ()
processAttachments = traverse_ handle
  where handle (Result x) = lift (logDebug x) >> convertAttachment x
        handle error      = lift $ logWarning error

convertAttachment :: Attachment -> StateT AttachmentsState App ()
convertAttachment (Attachment body) = addAttachment body
convertAttachment (Document DocumentBody {..}) = do
  peerId <- gets asPeerId
  file <- lift $ request $ GetFile dUrl
  uploadServer <- lift $ requestAndDecode $ GetUploadServer "doc" peerId
  case (file, uploadServer) of
    (Result r, Result (Success (UploadServer url))) -> do
        request <- lift $ requestAndDecode $ UploadFile r url dTitle
        case request of
          Result f@(FileUploaded x) -> lift (logDebug f) >> saveFile x dTitle
          error -> lift $ logWarning error
    (Result r, error) -> lift $ logWarning error

saveFile :: Text -> Text -> StateT AttachmentsState App ()
saveFile file name = do
  lift $ logDebug ("in save document" :: Text)
  lift (requestAndDecode $ SaveFile file name) >>= handle
  where handle :: Result (Response FileSaved)
               -> StateT AttachmentsState App ()
        handle r@(Result (Success x)) = do
          lift $ logDebug r
          lift $ logDebug x
          addAttachment x
        handle error = lift $ logWarning error

start, shutdown :: Text
start    = "Application getting started"
shutdown = "Application shut down"
