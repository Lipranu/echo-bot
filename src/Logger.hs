{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Logger where

import Control.Concurrent.MVar        (MVar, putMVar, takeMVar)
import Control.Monad.Reader
import Control.Monad.State            (StateT, evalStateT)
import Data.Text                      (Text, pack)
import Data.Text.IO                   (putStrLn)
import Data.Time                      (UTCTime, getCurrentTime)
import Prelude                 hiding (log, putStrLn)

class Has a r where
  getter :: r -> a

instance Has a a where
  getter = id

type Lock = MVar ()

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

data Env = Env
  { logPriority :: Priority
  , lock        :: Lock
  }

instance Has Priority Env where
  getter = logPriority

instance Has Lock Env where
  getter = lock

data State = State
  { sdf :: Int }

class Monad m => MonadTime m where
  getTime :: m UTCTime

class MonadTime m => MonadLog m where
  logger :: Priority -> Text -> m ()

type App = ReaderT Env (StateT State IO)

instance MonadLog App where
  logger lvl msg = showLog lvl msg >>= liftIO . putStrLn

instance MonadTime App where
  getTime = liftIO $ getCurrentTime

showt :: Show a => a -> Text
showt = pack . show

showLog :: MonadTime m => Priority -> Text -> m Text
showLog lvl msg =
  (<>) . showt
  <$> getTime
  <*> pure (" - [" <> showt lvl <> "]:\n" <> msg <> "\n")

logPure :: (Has Priority r, MonadReader r m, MonadLog m)
      => Priority
      -> Text
      -> m ()
logPure lvl msg = do
  priority <- asks getter
  unless (priority > lvl) $ logger lvl msg

log :: ( Has Priority r
       , Has Lock r
       , MonadReader r m
       , MonadLog m
       , MonadIO m
       )
    => Priority
    -> Text
    -> m ()
log lvl msg = do
  lock <- asks getter
  _ <- liftIO $ takeMVar lock
  logPure lvl msg
  liftIO $ putMVar lock ()

logDebug, logInfo, logWarning, logError
  :: ( Has Priority r
     , Has Lock r
     , MonadReader r m
     , MonadLog m
     , MonadIO m
     )
  => Text
  -> m ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning
logError   = log Error

runApp :: App a -> Env -> State -> IO a
runApp app env state = evalStateT (runReaderT app env) state
