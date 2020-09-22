{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module App.Vk ( mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has
import Infrastructure.Logger    ( Logger, Lock, mkLogger )
import Infrastructure.Requester ( Requester, mkRequester )

import App.Shared
import App.Shared.Routes
import App.Shared.Config hiding ( Config )

import App.Vk.Config
import App.Vk.Routes

import qualified App.Shared.Config     as Shared
import qualified Infrastructure.Logger as Logger

import Control.Monad.Catch  ( catches )
import Control.Monad.Reader ( runReaderT )
import Data.IORef           ( IORef )
import Network.HTTP.Client  ( Manager )

-- TYPES AND INSTANCES -----------------------------------------------------

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
  >> (getLongPollServer >>= getUpdates) `catches` handlers
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
