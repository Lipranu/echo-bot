{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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

import Control.Exception      ( try )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( ReaderT (..), MonadReader )
import Data.Aeson             ( (.:) )
import Data.Time              ( getCurrentTime )
import Data.IORef             ( IORef )
import Network.HTTP.Client    ( Manager, httpLbs )

import qualified Data.Aeson.Extended  as Aeson
import qualified Data.Text.IO         as TextIO

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
  , envLogger        :: Logger App
  , envLock          :: Lock
  , envRequester     :: Requester App
  , envHelpText      :: HelpText
  , envRepeatText    :: RepeatText
  }

instance Has Lock                Env where getter = envLock
instance Has (Logger App)        Env where getter = envLogger
instance Has (Requester App)     Env where getter = envRequester
instance Has Token               Env where getter = envToken
instance Has Group               Env where getter = envGroup
instance Has DefaultRepeat       Env where getter = envDefaultRepeat
instance Has HelpText            Env where getter = envHelpText
instance Has RepeatText          Env where getter = envRepeatText
instance Has (IORef Repetitions) Env where getter = envRepetitions

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
app = start >> getLongPollServer

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
runApp = runReaderT (unApp app)
