{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import Control.Monad.Reader        ( ReaderT (..), MonadReader
                                   , runReaderT, lift )
import Control.Monad.State         ( StateT (..), execStateT )
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

type HasLogger r m = (Has (Logger m) r, MonadReader r m, MonadTime m)

type MonadEffects r m =
  ( Has (Logger m) r
  , Has (Requester m) r
  , MonadTime m
  , MonadRequester m
  , MonadReader r m
  )

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
app = logInfo start >> getLongPollServer

mkApp :: Config -> Logger.Config -> Lock -> Manager -> Env
mkApp Config {..} cLogger lock = Env cToken cGroup logger lock . mkRequester
  where logger = mkLogger cLogger "Vk"

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)

getLongPollServer :: App ()
getLongPollServer = handleErrorRequest
  GetLongPollServer
  (getUpdates . mkGetUpdates)

getUpdates :: GetUpdates -> App ()
getUpdates gu = handleErrorRequest gu (routeUpdates gu)

routeUpdates :: GetUpdates -> Updates -> App ()
routeUpdates gu (Updates upd ts) = do
    traverseHandle processUpdate $ parse <$> upd
    getUpdates gu { guTs = ts }
routeUpdates gu (OutOfDate ts) = getUpdates gu { guTs = Text.showt ts }
routeUpdates _ _               = getLongPollServer

processUpdate :: Update -> App ()
processUpdate (NewMessage m) = processNewMessage m
processUpdate err = logWarning err

processNewMessage :: Message -> App ()
processNewMessage message = do
  aState   <- execStateT
    (traverseHandle routeAttachment $ parse <$> mAttachments message) $
    mkState message
  sendMessage <- mkSendMessage message aState <$> liftIO randomIO
  logInfo sendMessage
  handle =<< request sendMessage
  where handle :: Result LBS.ByteString -> App ()
        handle (Result v) = logDebug $ decodeUtf8 $ LBS.toStrict v
        handle (RequestError error) = logWarning error

routeAttachment :: Attachment -> StateT AttachmentsState App ()
routeAttachment (Attachment body) = logDebug body >> addAttachment body
routeAttachment (Wall       body) = logDebug body >> addAttachment body
routeAttachment (Document   body) = logDebug body >> processDocument body
routeAttachment (Sticker      id) = addSticker id

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
          uploadDocument $ mkUploadFile us file doc
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
          saveFile $ mkSaveFile uFile file
        handle error = lift $ logWarning error

saveFile :: SaveFile -> StateT AttachmentsState App ()
saveFile sf = do
  lift $ logInfo sf
  lift (requestAndDecode sf) >>= handle
  where handle :: Result (Response FileSaved)
               -> StateT AttachmentsState App ()
        handle (Result (Success x)) = lift (logDebug x) >> addAttachment x
        handle error = lift $ logWarning error

-- TODO: move all handlers to shared module
handle :: ( HasLogger r m, Loggable a)
       => (Result (Response a) -> m ())  -- logger for input
       -> m ()                           -- additional logger
       -> (a -> m ())                    -- function for handled input
       -> Result (Response a)            -- input
       -> m ()                           -- phantom result
handle _ _ route (Result (Success x)) = logDebug x >> route x
handle logger1 logger2 _ error = logger1 error >> logger2

handleR :: ( HasLogger r m, Loggable a)
       => (Result a -> m ())  -- logger for input
       -> m ()                           -- additional logger
       -> (a -> m ())                    -- function for handled input
       -> Result a            -- input
       -> m ()                           -- phantom result
handleR _ _ route (Result x) = logDebug x >> route x
handleR logger1 logger2 _ error = logger1 error >> logger2

handleWarningR :: ( HasLogger r m, Loggable a)
       => (a -> m ())                    -- function for handled input
       -> Result a
       -> m ()                           -- phantom result
handleWarningR = handleR logWarning (return ())

handleError, handleWarning
  :: ( HasLogger r m, Loggable input)
  => (input -> m ())
  -> Result (Response input)
  -> m ()
handleError   = handle logError (logError shutdown)
handleWarning = handle logWarning (return ())

requestWithLog ::
  ( MonadEffects r m
  , Loggable input
  , ToRequest m r input
  , Aeson.FromJSON output
  ) => input
    -> m (Result (Response output))
requestWithLog x = logInfo x >> requestAndDecode x

handleErrorRequest, handleWarningRequest ::
  ( MonadEffects r m
  , Loggable input
  , Loggable output
  , ToRequest m r input
  , Aeson.FromJSON output
  ) => input
    -> (output -> m ())
    -> m ()
handleErrorRequest   x f = requestWithLog x >>= handleError f
handleWarningRequest x f = requestWithLog x >>= handleWarning f

traverseHandle ::
  ( HasLogger r m
  , Loggable input
  ) => (input -> m ())
    -> [Result input]
    -> m ()
traverseHandle f = traverse_ (handleWarningR f)
start, shutdown :: Text
start    = "Application getting started"
shutdown = "Application shut down"
