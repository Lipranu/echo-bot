{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}

module Logger where

import           Control.Applicative            ((<|>), empty)
import           Control.Concurrent.Async                                --TEST
import           Control.Monad.Reader
import           Control.Monad.State            (StateT, evalStateT)     --MOVE
import           Data.Text                      (Text)
import           Data.Time                      (UTCTime, getCurrentTime)
import           Prelude                 hiding (log)
import           System.IO                                               --TEST

import qualified Data.Aeson as Aeson
import qualified Data.Text as                     Text
import qualified Data.Text.IO  as                     TextIO

-------------------------------------Logger------------------------------------

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

class Monad m => MonadTime m where
  getTime :: m UTCTime

class Monad m => MonadLogger m where
  logConsole :: Text -> m ()
  logFile    :: FilePath -> Text -> m ()

data Config = Config
  { cEnable   :: Maybe Bool
  , cPriority :: Maybe Priority
  , cShowTime :: Maybe Bool
  , cShowMode :: Maybe Bool
  , cFilePath :: Maybe FilePath
  } deriving Show

instance Semigroup Config where
  c1 <> c2 = Config
    (cEnable   c1 <|> cEnable   c2)
    (cPriority c1 <|> cPriority c2)
    (cShowTime c1 <|> cShowTime c2)
    (cShowMode c1 <|> cShowMode c2)
    (cFilePath c1 <|> cFilePath c2)

instance Monoid Config where
  mempty = Config empty empty empty empty empty

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

type HasLogger r m = (Has (Logger m) r, MonadReader r m)

showMessage :: Message -> Text
showMessage Message {..} = priority <> mode <> time <> message
    where
      priority = "[" <> Text.pack (show mPriority) <> "]"

      message  = ": " <> mText

      mode     = case mMode of
        Nothing -> ""
        Just m  -> " {" <> m <> "}"

      time     = case mTime of
        Nothing -> ""
        Just t  -> " (" <> Text.pack (show t) <> ")"

log :: (HasLogger r m, MonadTime m) => Priority -> Text -> m ()
log lvl msg = do
  a <- asks getter
  runLogger a $ Message lvl msg Nothing Nothing

logDebug, logInfo, logWarning, logError
  :: (HasLogger r m, MonadTime m)
  => Text
  -> m ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning
logError   = log Error

timeLogger :: MonadTime m => Logger m -> Logger m
timeLogger (Logger l) = Logger $ \m -> do
  time <- getTime
  l $ m { mTime = Just time }

modeLogger :: Text -> Logger m -> Logger m
modeLogger t (Logger l) = Logger $ \m -> l $ m { mMode = Just t }

filterLogger :: Monad m => Priority -> Logger m -> Logger m
filterLogger p (Logger a) = Logger $ \m -> when (mPriority m >= p) $ a m

consoleLogger :: MonadLogger m => Logger m
consoleLogger = Logger $ logConsole . showMessage

fileLogger :: MonadLogger m => FilePath -> Logger m
fileLogger path = Logger $ \m ->
  logFile path $ showMessage $ m { mText = mText m <> "\n" }

mkLogger :: MonadTime m => Priority -> Text -> Logger m -> Logger m
mkLogger lvl t
  = filterLogger lvl
  . modeLogger t
  . timeLogger

------------------------------TO MOVE------------------------------

data Env = Env
  { logger :: Logger App
  }

data State = State
  { sdf :: Int }

class Has a r where
  getter :: r -> a

instance Has a a where
  getter = id

type App = ReaderT Env (StateT State IO)

instance MonadLogger App where
  logConsole = liftIO . TextIO.putStrLn
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime App where
  getTime = liftIO $ getCurrentTime

runApp :: App a -> Env -> State -> IO a
runApp app env state = evalStateT (runReaderT app env) state

instance Has (Logger App) Env where
  getter = logger

------------------------------TEST---------------------------------

test :: IO ()
test = do
  hSetBuffering stdout LineBuffering
  let l1   = (mkLogger Info "Slack" consoleLogger)
          <> (mkLogger Debug "Slack" $ fileLogger "log/log")
      l2   = (mkLogger Debug "Vk" consoleLogger)
          <> (mkLogger Debug "Vk" $ fileLogger "log/log")

      env1 = Env l1
      env2 = Env l2
  concurrently (runApp app env1 (State 0)) (runApp app env2 (State 0))
  return ()

app :: App ()
app = do
  log Debug "Message 1"
  log Info "Message 2"
  log Warning "Message 3"
  log Error "Message 4"
