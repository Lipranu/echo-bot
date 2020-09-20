{-# LANGUAGE RecordWildCards   #-}

module App ( run ) where

import Config

import qualified App.Telegram as Telegram
import qualified App.Vk       as Vk


import Control.Concurrent.Async ( concurrently_ )
import Control.Concurrent.MVar  ( newMVar )
import Network.HTTP.Client.TLS  ( newTlsManager )
import Data.IORef               ( newIORef )
import Data.Aeson               ( eitherDecodeFileStrict )

run :: IO ()
run = do
  config <- eitherDecodeFileStrict "bot.conf.local"
  case (config :: Either String Config) of
    Left l -> putStrLn l
    Right Config {..} -> do
      lock    <- newMVar ()
      manager <- newTlsManager
      vkMap   <- newIORef mempty
      let vk  = Vk.mkApp       cVk       cShared cLogger lock vkMap manager
          tel = Telegram.mkApp cTelegram cLogger lock manager
      concurrently_ (Vk.runApp vk) (Telegram.runApp tel)
