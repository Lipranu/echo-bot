{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleContexts      #-}

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
import Control.Monad.Catch ( Handler (..), MonadThrow, MonadCatch )
import Control.Monad       ( foldM )

getUpdates :: (TelegramReader r m, MonadEffects r m, MonadThrow m)
           => GetUpdates -> m ()
getUpdates gu = withLog fromResponseR gu
  >>= processUpdates
  >>= getUpdates . GetUpdates

processUpdates :: MonadEffects r m => [Update] -> m (Maybe Integer)
processUpdates xs = foldM processUpdate Nothing xs

processUpdate :: MonadEffects r m
              => Maybe Integer
              -> Update
              -> m (Maybe Integer)
processUpdate current p@(Post id) = do
  logDebug p
  return (max current $ Just id)

fromResponseR
  :: forall output input env m
   . (ToRequest m input, FromJSON output, MonadThrow m, HasRequester env m)
  => input
  -> m output
fromResponseR = fromResponse @ResponseException @output
