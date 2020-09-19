{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Vk ( Config, mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import App.Shared               hiding ( Config )
import App.Vk.Internal
import App.Vk.Routes
import Infrastructure.Logger           ( Logger, mkLogger )
import Infrastructure.Requester        ( Requester, mkRequester )
import Internal

import qualified App.Shared            as Shared
import qualified Infrastructure.Logger as Logger

import Control.Monad.Catch  ( catches )
import Control.Monad.Reader ( runReaderT )
import Data.Aeson           ( FromJSON (..), (.:), withObject )
import Data.IORef           ( IORef )
import Network.HTTP.Client  ( Manager )

-- TYPES AND INSTANCES -----------------------------------------------------

data Config = Config
  { cToken :: Token
  , cGroup :: Group
  }

instance FromJSON Config where
  parseJSON = withObject "Vk.Config" $ \o -> Config
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

-- FUNCTIONS ---------------------------------------------------------------

app :: App Env ()
app = start
   >> (getLongPollServer >>= getUpdates processUpdates) `catches` handlers
   >> shutdown

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
