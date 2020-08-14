{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Logger.Internal
  ( Config
  , Logger
  , MonadLogger (..)
  , MonadTime (..)

  , logDebug
  , logError
  , logInfo
  , logWarning
  , mkLogger

  , log
  , concurrentLogger
  , consoleLogger
  , enableLogger
  , fileLogger
  , filterLogger
  , modeLogger
  , timeLogger
  ) where

import           Internal

import           Control.Concurrent.MVar     ( takeMVar
                                             , putMVar
                                             )
import           Control.Monad.Reader        ( MonadReader
                                             , MonadIO
                                             , asks
                                             , when
                                             , liftIO
                                             )
import           Data.Aeson                  ( FromJSON
                                             , (.:?)
                                             , (.:)
                                             , parseJSON
                                             , withObject
                                             , withText
                                             )
import           Data.Text                   ( Text
                                             , pack
                                             , unpack
                                             )
import           Data.Time                   ( UTCTime )
import           GHC.Generics                ( Generic )
import           Prelude              hiding ( log )

class Monad m => MonadTime m where
  getTime :: m UTCTime

class Monad m => MonadLogger m where
  logConsole :: Text -> m ()
  logFile    :: FilePath -> Text -> m ()

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

instance FromJSON Priority where
  parseJSON = withText "FromJSON Logger.Priority" $ \t ->
    case t of
      "debug"   -> pure Debug
      "info"    -> pure Info
      "warning" -> pure Warning
      "error"   -> pure Error
      _         -> fail $ "Unknown priority: " ++ unpack t

data Options = Options
  { sEnable   :: Maybe Bool
  , sPriority :: Maybe Priority
  , sShowTime :: Maybe Bool
  , sShowMode :: Maybe Bool
  } deriving (Generic, Show)

instance FromJSON Options where
  parseJSON = parseJsonDrop

data Config = Config
  { consoleOptions :: Options
  , fileOptions    :: Options
  , logFilePath    :: Maybe FilePath
  } deriving Show

instance FromJSON Config where
  parseJSON = withObject "FromJSON Logger.Config" $ \o -> Config
    <$> o .: "console_logger"
    <*> o .: "file_logger"
    <*> (o .: "file_logger" >>= (.:? "file_path"))

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

      message  = ": " <> unpack mText <> "\n"

      mode     = case mMode of
        Nothing -> ""
        Just m  -> " {" <> unpack m <> "}"

      time     = case mTime of
        Nothing -> ""
        Just t  -> " (" <> show t <> ")"

newtype Logger m = Logger { runLogger :: Message -> m () }

instance Applicative m => Semigroup (Logger m) where
  Logger a1 <> Logger a2 = Logger $ \m -> a1 m *> a2 m

instance Applicative m => Monoid (Logger m) where
  mempty = Logger $ \_ -> pure ()

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
timeLogger :: MonadTime m => Maybe Bool -> Logger m -> Logger m
timeLogger (Just True) (Logger l) = Logger $ \m -> do
  time <- getTime
  l $ m { mTime = Just time }
timeLogger _           l          = l

modeLogger :: Maybe Bool -> Text -> Logger m -> Logger m
modeLogger Nothing      t l          = modeLogger (Just True) t l
modeLogger (Just False) _ l          = l
modeLogger (Just True)  t (Logger l) = Logger $ \m -> l $ m { mMode = Just t }

filterLogger :: Monad m => Maybe Priority -> Logger m -> Logger m
filterLogger (Just p) (Logger l) = Logger $ \m ->
  when (mPriority m >= p) $ l m
filterLogger Nothing  l          = filterLogger (Just Warning) l

enableLogger :: Applicative m => Maybe Bool -> Logger m -> Logger m
enableLogger (Just False) _ = mempty
enableLogger _            l = l

consoleLogger :: MonadLogger m => Logger m
consoleLogger = Logger $ logConsole . pack . show

fileLogger :: MonadLogger m => Maybe FilePath -> Logger m
fileLogger (Just path) = Logger $ logFile path . pack . show
fileLogger Nothing     = mempty

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
            , MonadIO m)
         => Config
         -> Text
         -> Logger m
mkLogger Config {..} mode
   = builder consoleOptions consoleLogger
  <> builder fileOptions (fileLogger logFilePath)
  where builder Options {..} = enableLogger sEnable
                             . filterLogger sPriority
                             . modeLogger sShowMode mode
                             . timeLogger sShowTime
                             . concurrentLogger
