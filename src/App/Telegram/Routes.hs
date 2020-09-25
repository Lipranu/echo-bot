{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module App.Telegram.Routes
  ( getUpdates
  , GetUpdates (..)
  ) where

-- IMPORTS --------------------------------------------------------------------

import App.Telegram.Requests
import App.Telegram.Responses
import App.Telegram.Config    ( TelegramReader )

import App.Shared.Routes

import Infrastructure.Has
import Infrastructure.Logger
import Infrastructure.Requester

import Data.Aeson          ( FromJSON, Value )
import Control.Monad.Catch ( Handler (..), MonadThrow, MonadCatch, catch )
import Control.Monad       ( foldM )
import Data.Text.Extended  ( showt )

instance Loggable [Value] where
  toLog xs = "Get updates: " <> showt (length xs)

instance HasPriority [Value] where logData = logInfo . toLog

getUpdates :: (TelegramReader r m, MonadEffects r m, MonadThrow m, MonadCatch m)
           => GetUpdates -> m ()
getUpdates gu = withLog fromResponseR gu
  >>= convertRawUpdates
  >>= processUpdates
  >>= getUpdates . GetUpdates

convertRawUpdates :: (MonadCatch m, MonadEffects r m) => [Value] -> m [Update]
convertRawUpdates = foldM convert []
  where convert xs x = (parse x >>= return . (:xs)) `catch` handle xs
        handle  xs e = logData (e :: DecodeException) >> return xs

processUpdates :: (MonadEffects r m) => [Update] -> m (Maybe Integer)
processUpdates = foldM processUpdate Nothing

processUpdate :: MonadEffects r m
              => Maybe Integer
              -> Update
              -> m (Maybe Integer)
processUpdate current p@(Update id _) = do
  logDebug p
  return (max current $ Just id)

fromResponseR
  :: forall output input env m
   . (ToRequest m input, FromJSON output, MonadThrow m, HasRequester env m)
  => input
  -> m output
fromResponseR = fromResponse @ResponseException @output
