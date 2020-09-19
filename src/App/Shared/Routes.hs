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

type MonadEffects env m =
  ( Has (Requester m) env
  , MonadRequester m
  , MonadReader env m
  , HasLogger env m
  )

-- FUNCTIONS ---------------------------------------------------------------

logInput :: (HasPriority input, HasLogger env m)
         => (input -> m output)
         -> input
         -> m output
logInput f x = logData x >> f x

logOutput :: (HasPriority output, HasLogger env m)
          => output
          -> m output
logOutput x = logData x >> pure x

withLog :: (HasPriority input, HasPriority output, HasLogger env m)
        => (input -> m output)
        -> input
        -> m output
withLog f x = logInput f x >>= logOutput

handleResponse :: ( Exception  error
                  , FromJSON   error
                  , FromJSON   output
                  , MonadThrow m
                  )
               => (Response error output)
               -> m output
handleResponse (Success x) = return x
handleResponse (Error   e) = throwM e

fromResponse :: forall error output input env m
              . ( Exception    error
                , FromJSON     error
                , FromJSON     output
                , HasRequester env m
                , MonadThrow   m
                , ToRequest    m input
                )
             => input
             -> m output
fromResponse x = requestAndDecode x >>= handleResponse @error

sharedHandlers :: HasLogger env m => [Handler m () ]
sharedHandlers =
  [ Handler $ \(e :: HttpException)     -> logError $ toLog e
  , Handler $ \(e :: DecodeException)   -> logError $ toLog e
  ]

start, shutdown :: HasLogger env m => m ()
start    = logInfo  ("Application getting started" :: Text)
shutdown = logError ("Application shut down"       :: Text)
