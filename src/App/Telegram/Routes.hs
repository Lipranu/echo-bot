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

import App.Shared.Routes hiding ( fromResponse, fromValues, handlers )

import Infrastructure.Has
import Infrastructure.Logger
import Infrastructure.Requester

import qualified App.Shared.Routes as Shared
import Data.Aeson          ( FromJSON, Value )
import Control.Monad.Catch ( Handler (..), MonadThrow, MonadCatch, catch )
import Control.Monad       ( (>=>) )
import Data.Text.Extended  ( showt )
import Data.Semigroup      ( Sum (..) )

instance Loggable [Value] where
  toLog xs = "Get updates: " <> showt (length xs)

instance HasPriority [Value] where logData = logInfo . toLog

getUpdates
  :: ( TelegramReader r m
     , MonadEffects r m
     , MonadThrow m
     , MonadCatch m
     )
  => GetUpdates
  -> m ()
getUpdates = fromResponse
  >=> fromValues processUpdate
  >=> getUpdates . GetUpdates . check
  where check xs | null xs   = Nothing
                 | otherwise = getSum <$> maximum xs

processUpdate :: MonadEffects r m
              => Update
              -> m (Maybe (Sum Integer))
processUpdate p@(Update id _) = do
  logDebug p
  pure $ Just $ Sum id

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

fromValues
  :: ( FromJSON input
     , MonadCatch m
     , HasLogger env m
     , HasPriority input
     , Monoid output
     )
  => (input -> m output)
  -> [Value]
  -> m [output]
fromValues = Shared.fromValues handlers

handlers :: (Monoid output, HasLogger env m) => [Handler m output]
handlers = Handler (\(e :: ResponseException) -> logData e >> pure mempty)
  : Shared.handlers
