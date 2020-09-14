{-# LANGUAGE FlexibleContexts #-}

module App.Shared.Repetition
  ( Repetitions
  , Key
  , DefaultRepeat

  , getRepeats
  , putRepeats
  ) where

import Internal

import Control.Monad.Reader ( MonadReader )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Map  ( Map )
import Data.IORef ( IORef, readIORef, modifyIORef' )

import qualified Data.Map as Map

type Repetitions = Map Key Int
type Key = (Integer, Integer)

newtype DefaultRepeat = DefaultRepeat { unDefaultRepeat :: Int }

getRepeats ::
  ( MonadReader r m
  , Has (IORef Repetitions) r
  , Has DefaultRepeat r
  , MonadIO m
  , Has Key a
  ) => a
    -> m Int
getRepeats key = do
  def <- unDefaultRepeat    <$> obtain
  rep <- liftIO . readIORef =<< obtain
  return $ Map.findWithDefault def (getter key) (rep :: Repetitions)

putRepeats ::
  ( MonadReader r m
  , Has (IORef Repetitions) r
  , MonadIO m
  , Has Key a
  ) => a
    -> Int
    -> m ()
putRepeats key value = do
  map <- obtain
  liftIO $ modifyIORef'
    (map :: IORef Repetitions)
    (Map.insert (getter key) value)
