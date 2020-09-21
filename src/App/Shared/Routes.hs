{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Shared.Routes
  ( 
   MonadEffects
  , MonadRepetitions
  , Repetitions
  , fromResponse
  , getRepeats
  , putRepeats
  , sharedHandlers
  , shutdown
  , start
  , withLog
  , inputLog
  , outputLog
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Logger    hiding ( Priority (..) )
import Infrastructure.Requester
import Internal

import App.Shared.Responses
import App.Shared.Config

import Control.Monad.Catch    ( Handler (..), Exception, MonadThrow,  throwM )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( MonadReader )
import Data.Aeson.Extended    ( FromJSON )
import Data.IORef             ( IORef, readIORef, modifyIORef' )
import Data.Map.Strict        ( Map, lookup, insert )
import Data.Text              ( Text )
import Network.HTTP.Client    ( HttpException )

import Prelude         hiding ( lookup )

-- TYPES -------------------------------------------------------------------

type Repetitions = Map Key Int

type MonadEffects env m =
  ( HasRequester env m
  , HasLogger env m
  )

type MonadRepetitions r m =
  ( MonadReader r m
  , Has (IORef Repetitions) r
  , Has DefaultRepeat r
  , MonadIO m
  )

-- FUNCTIONS ---------------------------------------------------------------

getRepeats :: (MonadRepetitions r m, Has Key a) => a -> m (Maybe Int)
getRepeats key = do
  rep <- liftIO . readIORef =<< obtain @(IORef Repetitions)
  return $ lookup (getter key) rep

putRepeats :: (MonadRepetitions r m, Has Key a) => a -> Int -> m ()
putRepeats key value = do
  map <- obtain @(IORef Repetitions)
  liftIO $ modifyIORef' map $ insert (getter key) value

inputLog :: (HasPriority input, HasLogger env m)
         => (input -> m output)
         -> input
         -> m output
inputLog f x = logData x >> f x

outputLog :: (HasPriority output, HasLogger env m)
          => output
          -> m output
outputLog x = logData x >> pure x

withLog :: (HasPriority input, HasPriority output, HasLogger env m)
        => (input -> m output)
        -> input
        -> m output
withLog f x = inputLog f x >>= outputLog

handleResponse :: ( Exception  error
                  , FromJSON   error
                  , FromJSON   output
                  , MonadThrow m
                  )
               => Response error output
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
  [ Handler $ \(e :: HttpException)   -> logError $ toLog e
  , Handler $ \(e :: DecodeException) -> logError $ toLog e
  ]

start, shutdown :: HasLogger env m => m ()
start    = logInfo  ("Application getting started" :: Text)
shutdown = logError ("Application shut down"       :: Text)
