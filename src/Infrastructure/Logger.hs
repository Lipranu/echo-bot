{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Infrastructure.Logger
  ( Config (..)
  , HasLogger
  , Lock
  , LogDebug (..)
  , LogError (..)
  , LogText (..)
  , Loggable (..)
  , Logger (..)
  , MonadLogger (..)
  , MonadTime (..)
  , Options (..)
  , Priority (..)
  , log
  , logDebug
  , logError
  , logInfo
  , logWarning
  , mkLogText
  , mkLogTextLine
  , mkLogger
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger.LogText
import Infrastructure.Has

import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )
import Control.Monad           ( when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Control.Monad.Reader    ( MonadReader, lift )
import Control.Monad.State     ( StateT (..) )
import Data.Aeson              ( (.:?), (.!=) )
import Data.Text.Extended      ( Text )
import Data.Time               ( UTCTime )
import Network.HTTP.Client     ( HttpException )

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
  logData :: HasLogger r m => a -> m ()

-- TYPES AND INSTANCES -----------------------------------------------------

type Lock = MVar ()

type HasLogger r m =
  ( Has (Logger m) r
  , MonadReader r m
  , MonadTime m
  , MonadLogger m
  )

instance Loggable HttpException where
  logData e = logError $ mkLogText
    "HttpException:"
    [("Content", Text.showt e)]

newtype LogError a = LogError a

instance ToLayout a => Loggable (LogError a) where
  logData (LogError x) = logError $ layoutToText $ toLayout x

newtype LogDebug a = LogDebug a

instance ToLayout a => Loggable (LogDebug a) where
  logData (LogDebug x) = logDebug $ layoutToText $ toLayout x

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Eq, Ord)

instance Aeson.FromJSON Priority where
  parseJSON = Aeson.withText path $ \case
    "debug"   -> pure Debug
    "info"    -> pure Info
    "warning" -> pure Warning
    "error"   -> pure Error
    e         -> fail $ path <> ": unknown priority: " <> Text.unpack e
    where path = "Infrastructure.Logger.Priority"

data Options = Options
  { oEnable   :: Bool
  , oPriority :: Priority
  , oShowTime :: Bool
  , oShowMode :: Bool
  }

instance Aeson.FromJSON Options where
  parseJSON = Aeson.withObject "Infrastructure.Logger.Options"
    $ \o -> Options
    <$> o .:? "enable"    .!= True
    <*> o .:? "priority"  .!= Warning
    <*> o .:? "show_time" .!= False
    <*> o .:? "show_mode" .!= True

data Config = Config
  { consoleOptions :: Options
  , fileOptions    :: Options
  , logFilePath    :: FilePath
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Infrastructure.Logger.Config" $ \o -> do
    cl   <- o .:? "console_logger" .!= defaultOptions
    fl   <- o .:? "file_logger"    .!= defaultOptions
    path <- o .:? "log_path"       .!= "log"
    case path of
      ""   -> fail "Infrastructure.Logger.Config: empty log path"
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

newtype Logger m = Logger { runLogger :: Message -> m () }

instance Applicative m => Semigroup (Logger m) where
  Logger a1 <> Logger a2 = Logger $ \m -> a1 m *> a2 m

instance Applicative m => Monoid (Logger m) where
  mempty = Logger $ \_ -> pure ()

instance (HasLogger r m) => Has (Logger (StateT s m)) r where
  getter env = Logger $ \message ->  StateT $ \s ->
    (,s) <$> runLogger (getter env) message

instance MonadLogger m => MonadLogger (StateT s m) where
  logConsole   = lift . logConsole
  logFile path = lift . logFile path

instance MonadTime m => MonadTime (StateT s m) where
  getTime = lift getTime

-- FUNCTIONS ---------------------------------------------------------------

log :: HasLogger r m => Priority -> Text -> m ()
log lvl msg = do
  logger <- obtain
  runLogger logger message
  where message = Message
          { mPriority = lvl
          , mText     = msg
          , mMode     = Nothing
          , mTime     = Nothing
          }

logDebug, logInfo, logWarning, logError :: HasLogger r m => Text -> m ()
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
  runLogger logger m { mMode = Just t }
modeLogger False _ logger = logger

filterLogger :: Monad m => Priority -> Logger m -> Logger m
filterLogger p logger = Logger $ \m ->
  when (mPriority m >= p) $ runLogger logger m

enableLogger :: Applicative m => Bool -> Logger m -> Logger m
enableLogger True  logger = logger
enableLogger False _      = mempty

consoleLogger :: MonadLogger m => Logger m
consoleLogger = Logger $ logConsole . messageToLogEntry

fileLogger :: MonadLogger m => FilePath -> Logger m
fileLogger path = Logger $ logFile path . messageToLogEntry

concurrentLogger :: (Has Lock r, MonadReader r m, MonadLogger m, MonadIO m)
                 => Logger m
                 -> Logger m
concurrentLogger logger = Logger $ \m -> do
  lock <- obtain
  _ <- liftIO $ takeMVar lock
  runLogger logger m
  liftIO $ putMVar lock ()

mkLogger :: (Has Lock r, HasLogger r m, MonadIO m)
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

messageToLogEntry :: Message -> Text
messageToLogEntry Message {..} = Text.concat [priority, mode, time, message]
  where
    priority = Text.concat ["[", pToText, "]"]

    pToText  = case mPriority of
      Debug   -> "Debug"
      Info    -> "Info"
      Warning -> "Warning"
      Error   -> "Error"

    message  = Text.concat [": ", mText, "\n"]

    mode     = case mMode of
      Nothing -> ""
      Just "" -> ""
      Just m  -> Text.concat [" {", m, "}"]

    time     = case mTime of
      Nothing -> ""
      Just t  -> Text.concat [" (", Text.showt t, ")"]
