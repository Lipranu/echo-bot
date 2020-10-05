{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Shared.Routes
  ( MonadEffects
  , MonadRepetitions
  , Repetitions
  , Key
  , resultWithHandle
  , fromResponse
  , fromResponseWithHandle
  , getRepeats
  , putRepeats
  , handleValues
  , sharedHandlers
  , shutdown
  , start
  , withLog
  , withLog_
  , inputLog
  , outputLog
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has
import Infrastructure.Logger    hiding ( Priority (..) )
import Infrastructure.Requester

import App.Shared.Responses
import App.Shared.Config

import Control.Monad          ( foldM )
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

getRepeats :: (MonadRepetitions r m) => Key -> m (Maybe Int)
getRepeats key = do
  rep <- liftIO . readIORef =<< obtain @(IORef Repetitions)
  pure $ lookup key rep

putRepeats :: (MonadRepetitions r m) => Int -> Key -> m ()
putRepeats value key = do
  map <- obtain @(IORef Repetitions)
  liftIO $ modifyIORef' map $ insert key value

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

withLog_ :: (HasPriority input, HasPriority output, HasLogger env m)
         => (input -> m output)
         -> input
         -> m ()
withLog_ f x = inputLog f x >>= logData

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

fromResponseWithHandle
  :: forall error output input env m
   . ( Exception error
     , FromJSON error
     , FromJSON output
     , HasRequester env m
     , MonadCatch m
     , Monoid output
     , ToRequest m input
     )
  => (output -> [Handler m output])
  -> input
  -> m output
fromResponseWithHandle handlers x =
  fromResponse @error @output x `catches` handlers mempty

handleValues :: (Monoid output, FromJSON a, MonadCatch m)
             => (output -> [Handler m output])
             -> (a -> m output)
             -> [Value]
             -> m ()
handleValues handlers f = traverse_ handle
  where handle x = (parse x >>= f) `catches` handlers mempty

resultWithHandle :: (FromJSON a, MonadCatch m)
                 => ([output] -> [Handler m [output]])
                 -> (a -> m output)
                 -> [Value]
                 -> m [output]
resultWithHandle handlers f = foldM collect []
  where collect xs x = (parse x >>= f >>= pure . (:xs)) `catches` handlers xs

sharedHandlers :: HasLogger env m => output -> [Handler m output]
sharedHandlers output =
  [ Handler $ \(e :: HttpException)   -> logData e >> pure output
  , Handler $ \(e :: DecodeException) -> logData e >> pure output
  ]

start, shutdown :: HasLogger env m => m ()
start    = logInfo  ("Application getting started" :: Text)
shutdown = logError ("Application shut down"       :: Text)
