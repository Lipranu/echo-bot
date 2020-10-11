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
  , fromResponse_
  , fromValues
  , getRepeats
  , handlers
  , inputLog
  , outputLog
  , putRepeats
  , shutdown
  , start
  , traverseHandled
  , traverseHandled_
  , withLog
  , withLog_
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has
import Infrastructure.Logger    hiding ( Priority (..) )
import Infrastructure.Requester

import App.Shared.Responses
import App.Shared.Config

import Control.Monad          ( (>=>), foldM )
import Control.Monad.Catch    ( Exception, Handler (..), MonadCatch
                              , MonadThrow, catch, catches, throwM
                              )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader   ( MonadReader )
import Data.Aeson.Extended    ( FromJSON, Value )
import Data.Foldable          ( traverse_ )
import Data.Maybe             ( catMaybes )
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
     , HasPriority input
     , HasPriority output
     , MonadEffects env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m output
fromResponse = withLog (requestAndDecode >=> handleResponse @error @output)

fromResponse_
  :: forall error output input env m
   . ( Exception error
     , FromJSON error
     , FromJSON output
     , HasPriority input
     , HasPriority output
     , MonadEffects env m
     , MonadThrow m
     , ToRequest m input
     )
  => input
  -> m ()
fromResponse_ = withLog_ (requestAndDecode >=> handleResponse @error @output)

fromResponseH
  :: forall error output input env m
   . ( Exception error
     , FromJSON error
     , FromJSON output
     , HasPriority input
     , HasPriority output
     , MonadCatch m
     , MonadEffects env m
     , ToRequest m input
     )
  => (Maybe output -> [Handler m (Maybe output)])
  -> input
  -> m (Maybe output)
fromResponseH handlers x = (Just <$> fromResponse @error @output x)
  `catches` handlers Nothing

traverseHandled
  :: (MonadCatch m, HasLogger env m, HasPriority input)
  => (Maybe output -> [Handler m (Maybe output)])
  -> (input -> m (Maybe output))
  -> [input]
  -> m [output]
traverseHandled handlers f xs = catMaybes <$> traverse go xs
  where go x = inputLog f x `catches` handlers Nothing

traverseHandled_
  :: (MonadCatch m, HasLogger env m, HasPriority input)
  => (() -> [Handler m ()])
  -> (input -> m ())
  -> [input]
  -> m ()
traverseHandled_ handlers f = traverse_ go
  where go x = inputLog f x `catches` handlers ()

fromValues
  :: (FromJSON output, MonadCatch m, HasLogger env m)
  => [Value]
  -> m [output]
fromValues xs = reverse <$> foldM go [] xs
  where go  xs x = ((: xs) <$> parse x) `catch` err xs
        err xs e = logData (e :: DecodeException) >> pure xs

decodeHandler, httpHandler :: HasLogger env m => output -> Handler m output
decodeHandler x = Handler $ \(e :: DecodeException) -> logData e >> pure x
httpHandler   x = Handler $ \(e :: HttpException)   -> logData e >> pure x

handlers :: (HasLogger env m) => output -> [Handler m output]
handlers x = [httpHandler x, decodeHandler x]

start, shutdown :: HasLogger env m => m ()
start    = logInfo  ("Application getting started" :: Text)
shutdown = logError ("Application shut down"       :: Text)
