{-# LANGUAGE RecordWildCards   #-}

module App ( run ) where

import Config

import qualified App.Telegram as Telegram
import qualified App.Vk       as Vk


import Control.Concurrent.Async ( concurrently_ )
import Control.Concurrent.MVar  ( newMVar )
import Data.IORef               ( newIORef )
import Data.Yaml                ( ParseException, decodeFileEither)
import Network.HTTP.Client.TLS  ( newTlsManager )

run :: IO ()
run = do
  config <- decodeFileEither "config.yaml"
  case (config :: Either ParseException Config) of
    Left l -> print l
    Right Config {..} -> do
      lock    <- newMVar ()
      manager <- newTlsManager
      vkMap   <- newIORef mempty
      telMap  <- newIORef mempty
      let vk  = Vk.mkApp       cVk       cShared cLogger lock vkMap  manager
          tel = Telegram.mkApp cTelegram cShared cLogger lock telMap manager
      concurrently_ (Vk.runApp vk) (Telegram.runApp tel)
