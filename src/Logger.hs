{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}

module Logger
  ( Config (..)
  , Logger
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

-- IMPORTS ---------------------------------------------------------------------

import Internal                ( Has (..), Lock )

import Control.Concurrent.MVar ( takeMVar, putMVar )
import Control.Monad           ( when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Control.Monad.Reader    ( MonadReader, asks )
import Data.Aeson              ( (.:?), (.!=) )
import Data.Text               ( Text )
import Data.Time               ( UTCTime )

import qualified Data.Aeson as Aeson
import qualified Data.Text  as Text

import Prelude hiding ( log )

-- CLASSES ---------------------------------------------------------------------

class Monad m => MonadTime m where
  getTime :: m UTCTime

class Monad m => MonadLogger m where
  logConsole :: Text -> m ()
  logFile    :: FilePath -> Text -> m ()

-- TYPES AND INSTANCES ---------------------------------------------------------

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

instance Show Message where
  show Message {..} = priority <> mode <> time <> message
    where
      priority = "[" <> show mPriority <> "]"

      message  = ": " <> Text.unpack mText <> "\n"

      mode     = case mMode of
        Nothing -> ""
        Just "" -> ""
        Just m  -> " {" <> Text.unpack m <> "}"

      time     = case mTime of
        Nothing -> ""
        Just t  -> " (" <> show t <> ")"

newtype Logger m = Logger { runLogger :: Message -> m () }

instance Applicative m => Semigroup (Logger m) where
  Logger a1 <> Logger a2 = Logger $ \m -> a1 m *> a2 m

instance Applicative m => Monoid (Logger m) where
  mempty = Logger $ \_ -> pure ()

-- FUNCTIONS -------------------------------------------------------------------

log :: (Has (Logger m) r, MonadReader r m, MonadTime m)
    => Priority
    -> Text
    -> m ()
log lvl msg = do
  a <- asks getter
  runLogger a $ Message lvl msg Nothing Nothing

logDebug, logInfo, logWarning, logError
  :: (Has (Logger m) r, MonadReader r m, MonadTime m)
  => Text
  -> m ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning
logError   = log Error

-- Adds time to the logger message. If this option not specified
-- in the configuration, then it is disabled by default.
timeLogger :: MonadTime m => Bool -> Logger m -> Logger m
timeLogger True  (Logger l) = Logger $ \m -> do
  time <- getTime
  l $ m { mTime = Just time }
timeLogger False l          = l

modeLogger :: Bool -> Text -> Logger m -> Logger m
modeLogger True  t (Logger l) = Logger $ \m -> l $ m { mMode = Just t }
modeLogger False _ l          = l

filterLogger :: Monad m => Priority -> Logger m -> Logger m
filterLogger p (Logger l) = Logger $ \m -> when (mPriority m >= p) $ l m

enableLogger :: Applicative m => Bool -> Logger m -> Logger m
enableLogger True  l = l
enableLogger False _ = mempty

consoleLogger :: MonadLogger m => Logger m
consoleLogger = Logger $ logConsole . Text.pack . show

fileLogger :: MonadLogger m => FilePath -> Logger m
fileLogger path = Logger $ logFile path . Text.pack . show

concurrentLogger :: (Has Lock r, MonadReader r m, MonadLogger m, MonadIO m)
                 => Logger m
                 -> Logger m
concurrentLogger (Logger l) = Logger $ \m -> do
  lock <- asks getter
  _ <- liftIO $ takeMVar lock
  l m
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
