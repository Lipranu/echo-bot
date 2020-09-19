{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module App.Shared.Routes
  ( MonadEffects
  , withLog
  , fromResponse
  , sharedHandlers
  , start
  , shutdown
  ) where

-- IMPORTS -----------------------------------------------------------------

import Internal
import Infrastructure.Logger hiding ( Priority (..) )
import Infrastructure.Requester

import App.Shared.Responses

import Control.Monad.Reader ( MonadReader )
import Data.Aeson.Extended  ( FromJSON )
import Control.Monad.Catch  ( Handler (..), Exception, MonadThrow,  throwM )
import Network.HTTP.Client  ( HttpException )
import Data.Text            ( Text )

-- TYPES -------------------------------------------------------------------

type MonadEffects r m =
  ( Has (Requester m) r
  , MonadRequester m
  , MonadReader r m
  , HasLogger r m
  )

-- FUNCTIONS ---------------------------------------------------------------

withLog :: (HasPriority a, HasPriority b, HasLogger r m)
        => (a -> m b)
        -> a
        -> m b
withLog f x = logData x >> f x >>= \y -> logData y >> return y

handleResponse :: (Exception e, FromJSON e, FromJSON a, MonadThrow m)
               => (Response e a)
               -> m a
handleResponse (Success x) = return x
handleResponse (Error   e) = throwM e

fromResponse :: forall error output input env m
              . ( FromJSON     error
                , Exception    error
                , ToRequest    m input
                , FromJSON     output
                , MonadThrow   m
                , HasRequester env m
                )
             => input
             -> m output
fromResponse x = requestAndDecode x >>= handleResponse @error

sharedHandlers :: HasLogger r m => [Handler m () ]
sharedHandlers =
  [ Handler $ \(e :: HttpException)     -> logError $ toLog e
  , Handler $ \(e :: DecodeException)   -> logError $ toLog e
  ]

start, shutdown :: HasLogger r m => m ()
start    = logInfo  ("Application getting started" :: Text)
shutdown = logError ("Application shut down"       :: Text)
