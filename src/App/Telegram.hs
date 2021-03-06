{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module App.Telegram ( mkApp, runApp ) where

-- IMPORTS -----------------------------------------------------------------

import App.Telegram.Config
import App.Telegram.Routes

import App.Shared
import App.Shared.Config hiding ( Config )
import App.Shared.Routes        ( Repetitions, start, shutdown )

import Infrastructure.Has
import Infrastructure.Logger    ( Logger, Lock, mkLogger )
import Infrastructure.Requester

import qualified App.Shared.Config as Shared

import Control.Monad.Reader ( runReaderT )
import Data.IORef           ( IORef )
import Control.Monad.Catch  ( catches )

-- TYPES AND INSTANCES -----------------------------------------------------

data Env = Env
  { envToken         :: Token
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
instance Has DefaultRepeat         Env where getter = envDefaultRepeat
instance Has HelpText              Env where getter = envHelpText
instance Has RepeatText            Env where getter = envRepeatText
instance Has (IORef Repetitions)   Env where getter = envRepetitions

-- FUNCTIONS ---------------------------------------------------------------

app :: App Env ()
app = start >> loop >> shutdown
  where loop = getUpdates (GetUpdates Nothing) `catches` handlers ()

mkApp Config {..} Shared.Config {..} logger lock ref manager =
  let envLock          = lock
      envLogger        = mkLogger logger "Telegram"
      envToken         = cToken
      envRequester     = mkRequester manager
      envDefaultRepeat = cDefaultRepeat
      envRepeatText    = cRepeatText
      envHelpText      = cHelpText
      envRepetitions   = ref
   in Env {..}

runApp :: Env -> IO ()
runApp = runReaderT (unApp app)
