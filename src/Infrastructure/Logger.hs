{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module Infrastructure.Logger
  ( Config (..)
  , Logger (..)
  , Loggable (..)
  , MonadLogger (..)
  , MonadTime (..)
  , Options (..)
  , Priority (..)

  , log
  , logDebug
  , logError
  , logInfo
  , logWarning
  , mkLogger
  ) where

-- IMPORTS -----------------------------------------------------------------

import Internal

import Control.Concurrent.MVar      ( takeMVar, putMVar )
import Control.Monad                ( when )
import Control.Monad.IO.Class       ( MonadIO, liftIO )
import Control.Monad.Reader         ( MonadReader, lift )
import Control.Monad.State          ( StateT (..) )
import Data.Aeson                   ( (.:?), (.!=) )
import Data.Text.Extended           ( Text )
import Data.Text.Encoding           ( decodeUtf8 )
import Data.Time                    ( UTCTime )
import Network.HTTP.Client.Extended ( HttpException (..)
                                    , HttpExceptionContent (..)
                                    , responseStatus
                                    )
import Network.HTTP.Types.Status    ( Status (..) )

import qualified Data.Aeson         as Aeson
import qualified Data.Text.Extended as Text

import Prelude hiding ( log )

-- CLASSES -----------------------------------------------------------------

class Monad m => MonadTime m where
  getTime :: m UTCTime

class Monad m => MonadLogger m where
  logConsole :: Text -> m ()
  logFile    :: FilePath -> Text -> m ()

class Loggable a where
  toLog :: a -> Text

-- TYPES AND INSTANCES -----------------------------------------------------

instance Loggable Text   where toLog = id
instance Loggable String where toLog = Text.pack

instance Loggable HttpException where
  toLog (HttpExceptionRequest _ content) = toLog content

  toLog (InvalidUrlException url reason) = "InvalidUrlException\n\
    \ | Description: A URL is invalid for a given reason\n\
    \ | URL: "    <> Text.pack url <> "\n\
    \ | Reason: " <> Text.pack reason

instance Loggable HttpExceptionContent where
  toLog (StatusCodeException response _) = "StatusCodeException\n\
    \ | Description: Generated by the parseUrlThrow function when the \
    \server returns a non-2XX response status code\n\
    \ | Code: "    <> code <> "\n\
    \ | Message: " <> message
    where code    = Text.showt $ statusCode    $ responseStatus response
          message = decodeUtf8 $ statusMessage $ responseStatus response

  toLog (TooManyRedirects _) = "TooManyRedirects\n\
    \ | Description: The server responded with too many \
    \redirects for a request"

  toLog OverlongHeaders = "OverlongHeaders\n\
    \ | Description: Either too many headers, or too many \
    \total bytes in a single header, were returned by the server"

  toLog ResponseTimeout = "ResponseTimeout\n\
    \ | Description: The server took too long to return a response"

  toLog ConnectionTimeout = "ConnectionTimeout\n\
    \ | Description: Attempting to connect to the server timed out"

  toLog (ConnectionFailure e) = "ConnectionFailure\n\
    \ | Description: An exception occurred when trying \
    \to connect to the server\n\
    \ | Exception: " <> Text.showt e

  toLog (InvalidStatusLine bs) = "InvalidStatusLine\n\
    \ | Description: The status line returned by the \
    \server could not be parsed\n\
    \ | Content: " <> decodeUtf8 bs

  toLog (InvalidHeader bs) = "InvalidHeader\n\
    \ | Description: The given response header line could not be parsed\n\
    \ | Content: " <> decodeUtf8 bs

  toLog (InvalidRequestHeader bs) = "InvalidRequestHeader\n\
    \ | Description: The given request header is not compliant\n\
    \ | Content: " <> decodeUtf8 bs

  toLog (InternalException e) = "InternalException\n\
    \ | Description: An exception was raised by an underlying \
    \library when performing the request\n\
    \ | Content: " <> Text.showt e

  toLog (ProxyConnectException host port status) = "ProxyConnectException\n\
    \ | Description: A non-200 status code was returned when trying to \
    \connect to the proxy server on the given host and port\n\
    \ | Code: "    <> (Text.showt . statusCode) status    <> "\n\
    \ | Message: " <> (decodeUtf8 . statusMessage) status <> "\n\
    \ | Port: "    <> Text.showt port                     <> "\n\
    \ | Host: "    <> decodeUtf8 host

  toLog NoResponseDataReceived = "NoResponseDataReceived\n\
    \ | Description: No response data was received from the server \
    \at all. This exception may deserve special handling within the \
    \library, since it may indicate that a pipelining has been used, \
    \and a connection thought to be open was in fact closed"

  toLog TlsNotSupported = "TlsNotSupported\n\
    \ | Description: Exception thrown when using a Manager \
    \which does not have support for secure connections"

  toLog (WrongRequestBodyStreamSize expected actual)
    = "WrongRequestBodyStreamSize\n\
    \ | Description: The request body provided did not match \
    \the expected size\n\
    \ | Expected: " <> Text.showt expected <> "\n\
    \ | Actual: "   <> Text.showt actual

  toLog (ResponseBodyTooShort expected actual) = "ResponseBodyTooShort\n\
    \ | Description: The returned response body is too short\n\
    \ | Expected: " <> Text.showt expected <> "\n\
    \ | Actual: "   <> Text.showt actual

  toLog InvalidChunkHeaders = "InvalidChunkHeaders\n\
    \ | Description: A chunked response body had invalid headers"

  toLog IncompleteHeaders = "IncompleteHeaders\n\
    \ | Description: An incomplete set of response headers were returned"

  toLog (InvalidDestinationHost host) = "InvalidDestinationHost\n\
    \ | Description: The host we tried to connect to is invalid\n\
    \ | Host: " <> decodeUtf8 host

  toLog (HttpZlibException e) = "HttpZlibException\n\
    \ | Description: An exception was thrown when inflating a response body\n\
    \ | Content: " <> Text.showt e

  toLog (InvalidProxyEnvironmentVariable name value)
    = "InvalidProxyEnvironmentVariable\n\
    \ | Description: Values in the proxy environment variable were invalid\n\
    \ | Variable: " <> name <> "\n\
    \ | Value: "    <> value

  toLog ConnectionClosed = "ConnectionClosed\n\
    \ | Description: Attempted to use a Connection which was already closed"

  toLog (InvalidProxySettings content) = "InvalidProxySettings\n\
    \ | Description: Proxy settings are not valid\n\
    \ | Content: " <> content

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

instance Aeson.FromJSON Priority where
  parseJSON = Aeson.withText "Logger.Priority" $ \case
    "debug"   -> pure Debug
    "info"    -> pure Info
    "warning" -> pure Warning
    "error"   -> pure Error
    e         -> fail $ "Logger.Priority: unknown priority: " ++ Text.unpack e

data Options = Options
  { oEnable   :: Bool
  , oPriority :: Priority
  , oShowTime :: Bool
  , oShowMode :: Bool
  } deriving (Show, Eq)

instance Aeson.FromJSON Options where
  parseJSON = Aeson.withObject "Logger.Options" $ \o -> Options
    <$> o .:? "enable"    .!= True
    <*> o .:? "priority"  .!= Warning
    <*> o .:? "show_time" .!= False
    <*> o .:? "show_mode" .!= True

data Config = Config
  { consoleOptions :: Options
  , fileOptions    :: Options
  , logFilePath    :: FilePath
  } deriving (Show, Eq)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Logger.Config" $ \o -> do
    cl   <- o .:? "console_logger" .!= defaultOptions
    fl   <- o .:? "file_logger"    .!= defaultOptions
    path <- o .:? "log_path"       .!= "log"
    case path of
      ""   -> fail "Logger.Config: empty log path"
      path -> return $ Config cl fl path
    where defaultOptions = Options
            { oEnable   = True
            , oPriority = Warning
            , oShowTime = False
            , oShowMode = True
            }

data Message = Message
  { mPriority :: Priority
  , mText     :: Text
  , mTime     :: Maybe UTCTime
  , mMode     :: Maybe Text
  }

instance Loggable Message where
  toLog Message {..} = priority <> mode <> time <> message
    where
      priority = "[" <> Text.showt mPriority <> "]"

      message  = ": " <> mText <> "\n"

      mode     = case mMode of
        Nothing -> ""
        Just "" -> ""
        Just m  -> " {" <> m <> "}"

      time     = case mTime of
        Nothing -> ""
        Just t  -> " (" <> Text.showt t <> ")"

newtype Logger m = Logger { runLogger :: Message -> m () }

instance Applicative m => Semigroup (Logger m) where
  Logger a1 <> Logger a2 = Logger $ \m -> a1 m *> a2 m

instance Applicative m => Monoid (Logger m) where
  mempty = Logger $ \_ -> pure ()

instance (Has (Logger m) r, MonadReader r m)
  => Has (Logger (StateT s m)) r where
  getter env = Logger $ \message ->  StateT $ \s ->
    (,s) <$> runLogger (getter env) message

instance MonadLogger m => MonadLogger (StateT s m) where
  logConsole   = lift . logConsole
  logFile path = lift . logFile path

instance MonadTime m => MonadTime (StateT s m) where
   getTime = lift $ getTime

-- FUNCTIONS ---------------------------------------------------------------

log :: (Has (Logger m) r, MonadReader r m, MonadTime m, Loggable a)
    => Priority
    -> a
    -> m ()
log lvl msg = do
  logger <- obtain
  runLogger logger message
  where message = Message
          { mPriority = lvl
          , mText     = toLog msg
          , mMode     = Nothing
          , mTime     = Nothing
          }

logDebug, logInfo, logWarning, logError
  :: (Has (Logger m) r, MonadReader r m, MonadTime m, Loggable a)
  => a
  -> m ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning
logError   = log Error

timeLogger :: MonadTime m => Bool -> Logger m -> Logger m
timeLogger True  logger = Logger $ \m -> do
  time <- getTime
  runLogger logger $ m { mTime = Just time }
timeLogger False logger = logger

modeLogger :: Bool -> Text -> Logger m -> Logger m
modeLogger True  t logger = Logger $ \m ->
  runLogger logger $ m { mMode = Just t }
modeLogger False _ logger = logger

filterLogger :: Monad m => Priority -> Logger m -> Logger m
filterLogger p logger = Logger $ \m ->
  when (mPriority m >= p) $ runLogger logger m

enableLogger :: Applicative m => Bool -> Logger m -> Logger m
enableLogger True  logger = logger
enableLogger False _      = mempty

consoleLogger :: MonadLogger m => Logger m
consoleLogger = Logger $ logConsole . toLog

fileLogger :: MonadLogger m => FilePath -> Logger m
fileLogger path = Logger $ logFile path . toLog

concurrentLogger :: (Has Lock r, MonadReader r m, MonadLogger m, MonadIO m)
                 => Logger m
                 -> Logger m
concurrentLogger logger = Logger $ \m -> do
  lock <- obtain
  _ <- liftIO $ takeMVar lock
  runLogger logger m
  liftIO $ putMVar lock ()

mkLogger :: ( Has Lock r
            , MonadReader r m
            , MonadLogger m
            , MonadTime m
            , MonadIO m
            )
         => Config
         -> Text
         -> Logger m
mkLogger Config {..} mode
   = builder consoleOptions consoleLogger
  <> builder fileOptions (fileLogger logFilePath)
  where builder Options {..} = enableLogger oEnable
                             . filterLogger oPriority
                             . modeLogger   oShowMode mode
                             . timeLogger   oShowTime
                             . concurrentLogger
