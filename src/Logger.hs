{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DeriveGeneric               #-}

module Logger where

import           Control.Concurrent.Async                                --TEST
import           Control.Monad.Reader
import           Control.Monad.State            ( MonadState
                                                , StateT
                                                , evalStateT
                                                )                        --MOVE
import           Data.Aeson                     ( FromJSON, GFromJSON, GToJSON
                                                , Zero, Value, (.:?), (.:)
                                                )
import           Data.Aeson.Types              (Parser)
import qualified Data.ByteString.Lazy as BS
import           Data.Char                     (isLower, isUpper, toLower)
import           Data.Text                      (Text)
import           Data.Time                      (UTCTime, getCurrentTime)
import           Prelude                 hiding (log)
import           System.IO                                               --TEST
import           GHC.Generics                    (Generic, Rep)

import qualified Data.Aeson                  as Aeson
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as TextIO

-------------------------------------Logger------------------------------------

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

instance FromJSON Priority where
  parseJSON = Aeson.withText "FromJSON Logger.Priority" $ \t ->
    case t of
      "debug"   -> pure Debug
      "info"    -> pure Info
      "warning" -> pure Warning
      "error"   -> pure Error
      _         -> fail $ "Unknown priority: " ++ Text.unpack t

class Monad m => MonadTime m where
  getTime :: m UTCTime

class Monad m => MonadLogger m where
  logConsole :: Text -> m ()
  logFile    :: FilePath -> Text -> m ()

data Config = Config
  { consoleOptions :: Options
  , fileOptions    :: Options
  , logFilePath    :: Maybe FilePath
  } deriving Show

instance FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON Logger.Config" $ \o -> Config
    <$> o .: "console_logger"
    <*> o .: "file_logger"
    <*> (o .: "file_logger" >>= (.:? "file_path"))

data Options = Options
  { sEnable   :: Maybe Bool
  , sPriority :: Maybe Priority
  , sShowTime :: Maybe Bool
  , sShowMode :: Maybe Bool
  } deriving (Generic, Show)

instance FromJSON Options where
  parseJSON = parseJsonDrop

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
        Just m  -> " {" <> Text.unpack m <> "}"

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
filterLogger (Just p) (Logger a) = Logger $ \m ->
  when (mPriority m >= p) $ a m
filterLogger Nothing  l          = filterLogger (Just Warning) l

enableLogger :: Applicative m => Maybe Bool -> Logger m -> Logger m
enableLogger (Just False) _ = mempty
enableLogger _            l = l

consoleLogger :: MonadLogger m => Logger m
consoleLogger = Logger $ logConsole . Text.pack . show

fileLogger :: MonadLogger m => Maybe FilePath -> Logger m
fileLogger (Just path) = Logger $ logFile path . Text.pack . show
fileLogger Nothing     = mempty

mkLogger :: (MonadLogger m, MonadTime m) => Config -> Text -> Logger m
mkLogger Config {..} mode
   = builder consoleOptions consoleLogger
  <> builder fileOptions (fileLogger logFilePath)
  where builder Options {..} = enableLogger sEnable
                             . filterLogger sPriority
                             . modeLogger sShowMode mode
                             . timeLogger sShowTime

------------------------------TO MOVE------------------------------

newtype Env = Env
  { logger :: Logger App
  }

newtype State = State
  { sdf :: Int }

class Has a r where
  getter :: r -> a

instance Has a a where
  getter = id

newtype App a = App { unApp :: ReaderT Env (StateT State IO) a } deriving
  (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadState State)

instance MonadLogger App where
  logConsole = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime App where
  getTime = liftIO getCurrentTime

runApp :: App a -> Env -> State -> IO a
runApp app env = evalStateT (runReaderT (unApp app) env)

instance Has (Logger App) Env where
  getter = logger

parseJsonDrop :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
parseJsonDrop = Aeson.genericParseJSON Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldToJsonKey }

toJsonDrop :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
toJsonDrop = Aeson.genericToJSON Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldToJsonKey
  , Aeson.omitNothingFields = True
  }

fieldToJsonKey :: String -> String
fieldToJsonKey str
  | all isLower str = str
  | otherwise       = foldr f ""
                    $ (\(x:xs) -> toLower x : xs)
                    $ dropWhile isLower str
  where f x y | isUpper x = '_' : toLower x : y
              | otherwise = x : y

------------------------------TEST---------------------------------

test :: IO ()
test = do
  config <- BS.readFile "bot.conf"
--  case config of
--    Left e -> putStrLn e
--    Right r -> do
  let config' = Aeson.eitherDecode config :: Either String Config
  hSetBuffering stdout LineBuffering
  case config' of
    Left e -> putStrLn e
    Right r -> do
      let vk = mkLogger r "vk"
          te = mkLogger r "telegram"
      concurrently (runApp app (Env vk) (State 0)) (runApp app (Env te) (State 0))
      return ()

app :: App ()
app = do
  log Debug "Message 1"
  log Info "Message 2"
  log Warning "Message 3"
  log Error "Message 4"
