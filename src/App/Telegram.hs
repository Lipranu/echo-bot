{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module App.Telegram ( mkApp, runApp ) where

-- IMPORTS --------------------------------------------------------------------

import App.Telegram.Config
import App.Telegram.Requests
import App.Telegram.Responses

import App.Shared
import App.Shared.Config        hiding ( Config )
import App.Shared.Routes

import Infrastructure.Has
import Infrastructure.Logger    hiding ( Config )
import Infrastructure.Requester

import qualified App.Shared.Config as Shared

import Control.Monad.Reader ( ReaderT (..) )
import Data.IORef           ( IORef )

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
app = start

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

--getUpdates :: GetUpdates -> App ()
--getUpdates gu = do
--  logInfo gu
--  result <- requestAndDecode gu
--  case result of
--    Result (Succes v) -> do
--      logInfo v
--      proccessUpdates v
--    error -> do
--      logError error
--      logError ("Application shut down" :: Text)

--proccessUpdates :: [Update] -> App ()
--proccessUpdates xs = do
--  x <- foldM proccessUpdate Nothing xs
--  getUpdates $ GetUpdates x
--
--proccessUpdate :: Maybe Integer -> Update -> App (Maybe Integer)
--proccessUpdate current p@(Post id) = do
--  logDebug p
--  return (max current $ Just id)
