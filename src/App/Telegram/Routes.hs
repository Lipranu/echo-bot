{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module App.Telegram.Routes
  ( getUpdates
  , GetUpdates (..)
  ) where

-- IMPORTS -----------------------------------------------------------------

import App.Telegram.Requests
import App.Telegram.Responses
import App.Telegram.Config      ( TelegramReader )

import App.Shared.Routes hiding ( fromResponse, traverseHandled, handlers )

import Infrastructure.Has
import Infrastructure.Logger
import Infrastructure.Requester

import qualified App.Shared.Routes as Shared

import Data.Aeson          ( FromJSON )
import Control.Monad.Catch ( Handler (..), MonadThrow, MonadCatch )
import Control.Monad       ( (>=>) )

-- FUNCTIONS ---------------------------------------------------------------

getUpdates
  :: ( TelegramReader r m
     , MonadEffects r m
     , MonadThrow m
     , MonadCatch m
     )
  => GetUpdates
  -> m ()
getUpdates = fromResponse
  >=> fromValues . unUpdates
  >=> traverseHandled processUpdate
  >=> getUpdates . GetUpdates . check
  where check xs | null xs   = Nothing
                 | otherwise = Just $ maximum xs

processUpdate :: MonadEffects r m
              => Update
              -> m (Maybe Integer)
processUpdate p@(Update id _) = do
  logDebug p
  pure $ Just id

fromResponse
  :: forall output input env m
   . ( ToRequest m input
     , FromJSON output
     , MonadThrow m
     , HasPriority input
     , HasPriority output
     , MonadEffects env m
     )
  => input
  -> m output
fromResponse = Shared.fromResponse @ResponseException @output

traverseHandled
  :: (MonadCatch m, HasLogger env m, HasPriority input)
  => (input -> m (Maybe output))
  -> [input]
  -> m [output]
traverseHandled = Shared.traverseHandled handlers

handlers :: HasLogger env m => output -> [Handler m output]
handlers x = Handler (\(e :: ResponseException) -> logData e >> pure x)
  : Shared.handlers x
