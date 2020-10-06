{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Shared.Routes
  ( Key
  , MonadEffects
  , MonadRepetitions
  , Repetitions
  , fromResponse
  , fromResponseH
  , fromValues
  , fromValues_
  , getRepeats
  , handlers
  , inputLog
  , outputLog
  , putRepeats
  , shutdown
  , start
  , withLog
  , withLog_
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has
import Infrastructure.Logger    hiding ( Priority (..) )
import Infrastructure.Requester

import App.Shared.Responses
import App.Shared.Config

import Control.Monad          ( (>=>) )
import Control.Monad.Catch    ( Handler (..), Exception, MonadThrow
                              , MonadCatch, throwM, catches
                              )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( MonadReader )
import Data.Aeson.Extended    ( FromJSON, Value )
import Data.IORef             ( IORef, readIORef, modifyIORef' )
import Data.Map.Strict        ( Map, lookup, insert )
import Data.Text              ( Text )
import Data.Foldable          ( traverse_ )
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

getRepeats :: MonadRepetitions env m => Key -> m (Maybe Int)
getRepeats key = do
  rep <- obtain @(IORef Repetitions)
  map <- liftIO $ readIORef rep
  pure $ lookup key map

putRepeats :: MonadRepetitions env m => Int -> Key -> m ()
putRepeats value key = do
  map <- obtain @(IORef Repetitions)
  let f = insert key value
  liftIO $ modifyIORef' map f

inputLog
  :: (HasPriority input, HasLogger env m)
  => (input -> m output)
  -> input
  -> m output
inputLog f x = logData x >> f x

outputLog
  :: (HasPriority output, HasLogger env m)
  => output
  -> m output
outputLog x = logData x >> pure x

withLog
  :: (HasPriority input, HasPriority output, HasLogger env m)
  => (input -> m output)
  -> input
  -> m output
withLog f = inputLog f >=> outputLog

withLog_
  :: (HasPriority input, HasPriority output, HasLogger env m)
  => (input -> m output)
  -> input
  -> m ()
withLog_ f = inputLog f >=> logData

handleResponse
  :: (Exception error, FromJSON error, FromJSON output, MonadThrow m)
  => Response error output
  -> m output
handleResponse (Success x) = pure   x
handleResponse (Error   e) = throwM e

fromResponse
  :: forall error output input env m
   . ( Exception error
     , FromJSON error
     , FromJSON output
     , HasRequester env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m output
fromResponse = requestAndDecode >=> handleResponse @error

fromResponseH
  :: forall error output input env m
   . ( Exception error
     , FromJSON error
     , FromJSON output
     , HasRequester env m
     , MonadCatch m
     , Monoid output
     , ToRequest m input
     )
  => ([Handler m output])
  -> input
  -> m output
fromResponseH handlers x = fromResponse @error @output x `catches` handlers

fromValues_
  :: (FromJSON input, MonadCatch m)
  => ([Handler m ()])
  -> (input -> m ())
  -> [Value]
  -> m ()
fromValues_ handlers f = traverse_ go
  where go x = (parse >=> f) x `catches` handlers

fromValues
  :: (FromJSON input, MonadCatch m)
  => ([Handler m output])
  -> (input -> m output)
  -> [Value]
  -> m [output]
fromValues handlers f = traverse go
  where go x = (parse >=> f) x `catches` handlers

handlers :: (Monoid output, HasLogger env m) => [Handler m output]
handlers =
  [ Handler $ \(e :: HttpException)   -> logData e >> pure mempty
  , Handler $ \(e :: DecodeException) -> logData e >> pure mempty
  ]

start, shutdown :: HasLogger env m => m ()
start    = logInfo  ("Application getting started" :: Text)
shutdown = logError ("Application shut down"       :: Text)
