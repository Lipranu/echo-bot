{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
import Control.Monad.State         ( StateT, execStateT )
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
        handle (Result (Success lps)) = do
          logDebug lps
          getUpdates $ convert lps
        handle error = logError error >> logError shutdown

getUpdates :: GetUpdates -> App ()
getUpdates gu = do
  logInfo gu
  requestAndDecode gu >>= handle
  where handle :: Result Updates -> App ()
        handle (Result u) = logDebug u >> route u
        handle error      = logError error >> logError shutdown

        route :: Updates -> App ()
        route (Updates upd ts) = do
          processUpdates $ parse <$> upd
          getUpdates gu { guTs = ts }
        route (OutOfDate ts) = getUpdates gu { guTs = Text.showt ts }
        route rest           = getLongPollServer

processUpdates :: [Result Update] -> App ()
processUpdates = traverse_ handle
  where handle :: Result Update -> App ()
        handle (Result (NewMessage m)) = do
          logDebug m
          processNewMessage m
        handle error = logWarning error

processNewMessage :: Message -> App ()
processNewMessage message = do
  randomId <- liftIO randomIO :: App Int
  let attach = parse <$> mAttachments message
  result <- execStateT (processAttachments attach) $ mkState message
  let sendm  = convert message randomId result
  logInfo sendm
  handle =<< request sendm
  where handle :: Result LBS.ByteString -> App ()
        handle (Result v) = logDebug $ decodeUtf8 $ LBS.toStrict v
        handle (RequestError error) = logWarning error

processAttachments :: [Result Attachment] -> StateT AttachmentsState App ()
processAttachments = traverse_ handle
  where handle :: Result Attachment -> StateT AttachmentsState App ()
        handle (Result r) = lift (logDebug r) >> route r
        handle error = lift $ logWarning error

        route :: Attachment -> StateT AttachmentsState App ()
        route (Attachment body) = do
          lift $ logDebug body
          addAttachment body
        route (Document   body) = do
          lift $ logDebug body
          processDocument body

processDocument :: DocumentBody -> StateT AttachmentsState App ()
processDocument doc = do
  (gFile, gUploadServer) <- mkUploadRequests doc
  lift $ logInfo gFile
  file <- lift $ request gFile
  lift $ logInfo gUploadServer
  uploadServer <- lift $ requestAndDecode gUploadServer
  handle file uploadServer
  where handle :: Result LBS.ByteString
               -> Result (Response UploadServer)
               -> StateT AttachmentsState App ()
        handle (Result file) (Result (Success us)) = do
          lift $ logDebug us
          uploadDocument $ convert us file doc
        handle (Result _) error = lift $ logWarning error
        handle (RequestError error) (Result (Success _)) =
          lift $ logWarning error
        handle (RequestError error1) error2 = do
          lift $ logWarning error1
          lift $ logWarning error2

uploadDocument :: UploadFile -> StateT AttachmentsState App ()
uploadDocument uFile = do
  lift $ logInfo uFile
  lift (requestAndDecode uFile) >>= handle
  where handle :: Result FileUploaded -> StateT AttachmentsState App ()
        handle (Result result@(FileUploaded file)) = do
          lift $ logDebug result
          saveFile $ convert uFile file
        handle error = lift $ logWarning error

saveFile :: SaveFile -> StateT AttachmentsState App ()
saveFile sf = do
  lift $ logInfo sf
  lift (requestAndDecode sf) >>= handle
  where handle :: Result (Response FileSaved)
               -> StateT AttachmentsState App ()
        handle (Result (Success x)) = lift (logDebug x) >> addAttachment x
        handle error = lift $ logWarning error

start, shutdown :: Text
start    = "Application getting started"
shutdown = "Application shut down"
