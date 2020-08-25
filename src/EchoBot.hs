{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}

module EchoBot ( run ) where

import           Internal                        ( Has (..), Lock )
import qualified Logger
import qualified Request

import           Control.Concurrent.Async        ( concurrently )
import           Control.Concurrent.MVar         ( newMVar )
import           Control.Monad.Reader
import           Control.Monad.State             ( MonadState
                                                 , StateT
                                                 , evalStateT
                                                 )
import           Data.Time                       ( getCurrentTime )
import           System.IO                       ( BufferMode (..)
                                                 , hSetBuffering
                                                 , stdout
                                                 )
import qualified Data.Aeson                   as Aeson
import qualified Data.Text.IO                 as TextIO

data Env = Env
  { logger :: Logger.Logger App
  , lock   :: Lock
  }

newtype State = State Int

newtype App a = App { unApp :: ReaderT Env (StateT State IO) a } deriving
  (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadState State)

instance Logger.MonadLogger App where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance Logger.MonadTime App where
  getTime = liftIO getCurrentTime

instance Has (Logger.Logger App) Env where
  getter = logger

instance Has Lock Env where
  getter = lock

runApp :: App a -> Env -> State -> IO a
runApp app env = evalStateT (runReaderT (unApp app) env)

run :: IO ()
run = do
  lock <- newMVar ()
--  config <- BS.readFile "bot.conf"
  config <- Aeson.eitherDecodeFileStrict "bot.conf" :: IO (Either String Logger.Config)
  hSetBuffering stdout LineBuffering
  case config of
    Left  e -> putStrLn $ "config error" <> e
    Right r -> do
      let vk = Logger.mkLogger r "vk"
          te = Logger.mkLogger r "telegram"
      concurrently
        (runApp app (Env vk lock) (State 0))
        (runApp app (Env te lock) (State 0))
      return ()

app :: App ()
app = do
  liftIO $ Request.request
  Logger.logDebug "Message 1"
  Logger.logInfo "Message 2"
  Logger.logWarning "Message 3"
  Logger.logError "Message 4"
