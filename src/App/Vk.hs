{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import App.Vk.Routes
import App.Vk.Internal
import App.Shared.Repetition
import App.Shared            hiding ( Config )

import Infrastructure.Logger    hiding ( Config, Priority (..) )
import Infrastructure.Requester
import Internal

import qualified App.Shared            as Shared
import qualified Infrastructure.Logger as Logger

import Control.Monad.Catch    ( Handler (..), MonadThrow, MonadCatch, catches )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT (..), MonadReader )
import Data.Aeson             ( (.:) )
import Data.IORef             ( IORef )
import Data.Text.Extended     ( Text, showt )
import Data.Time              ( getCurrentTime )
import Network.HTTP.Client    ( HttpException (..), Manager, httpLbs )

import qualified Data.Aeson.Extended as Aeson
import qualified Data.Text.IO        as TextIO

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
  { envToken         :: Token
  , envGroup         :: Group
  , envDefaultRepeat :: DefaultRepeat
  , envRepetitions   :: IORef Repetitions
  , envLogger        :: Logger (App Env)
  , envLock          :: Lock
  , envRequester     :: Requester (App Env)
  , envHelpText      :: HelpText
  , envRepeatText    :: RepeatText
  }

instance Has Lock                  Env where getter = envLock
instance Has (Logger (App Env))    Env where getter = envLogger
instance Has (Requester (App Env)) Env where getter = envRequester
instance Has Token                 Env where getter = envToken
instance Has Group                 Env where getter = envGroup
instance Has DefaultRepeat         Env where getter = envDefaultRepeat
instance Has HelpText              Env where getter = envHelpText
instance Has RepeatText            Env where getter = envRepeatText
instance Has (IORef Repetitions)   Env where getter = envRepetitions

newtype App r a = App { unApp :: ReaderT r IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadCatch
           , MonadIO
           , MonadReader r
           , MonadThrow
           )

instance MonadLogger (App r) where
  logConsole   = liftIO . TextIO.putStr
  logFile path = liftIO . TextIO.appendFile path

instance MonadTime (App r) where
  getTime = liftIO getCurrentTime

instance MonadRequester (App r) where
  requester manager req = liftIO $ httpLbs req manager

-- FUNCTIONS ---------------------------------------------------------------

app :: App Env ()
app = start
   >> (getLongPollServer >>= getUpdates processUpdates) `catches` handlers
   >> shutdown

--handleLoggable :: [Handler (App Env) () ]
--handleLoggable =
--  [ Handler $ \(e :: HttpException) -> logError $ toLog e
--  , Handler $ \(e :: DecodeException) -> logError $ toLog e
--  ]

mkApp :: Config
      -> Shared.Config
      -> Logger.Config
      -> Lock
      -> IORef Repetitions
      -> Manager
      -> Env
mkApp Config {..} Shared.Config {..} logger lock ref manager =
  let envLock          = lock
      envLogger        = mkLogger logger "Vk"
      envToken         = cToken
      envGroup         = cGroup
      envRequester     = mkRequester manager
      envDefaultRepeat = cDefaultRepeat
      envRepeatText    = cRepeatText
      envHelpText      = cHelpText
      envRepetitions   = ref
   in Env {..}

runApp :: Env -> IO ()
runApp env = runReaderT (unApp app) env
