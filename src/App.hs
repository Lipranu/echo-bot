{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}

module App ( run ) where

import qualified App.Telegram             as Telegram
import qualified App.Vk                   as Vk
import qualified App.Shared               as Shared
import qualified Infrastructure.Logger    as Logger

import Control.Concurrent.Async ( concurrently_ )
import Control.Concurrent.MVar  ( newMVar )
import GHC.Generics             ( Generic )
import Network.HTTP.Client.TLS  ( newTlsManager )
import Data.IORef               ( newIORef )

import qualified Data.Aeson.Extended as Aeson

data Config = Config
  { cLogger   :: Logger.Config
  , cVk       :: Vk.Config
  , cTelegram :: Telegram.Config
  , cShared   :: Shared.Config
  } deriving Generic

instance Aeson.FromJSON Config where
  parseJSON = Aeson.parseJsonDrop

run :: IO ()
run = do
  config <- Aeson.eitherDecodeFileStrict "bot.conf.local"
  case (config :: Either String Config) of
    Left l -> putStrLn l
    Right Config {..} -> do
      lock    <- newMVar ()
      manager <- newTlsManager
      vkMap   <- newIORef mempty
      let vk  = Vk.mkApp       cVk       cShared cLogger lock vkMap manager
          tel = Telegram.mkApp cTelegram cLogger lock manager
      concurrently_ (Vk.runApp vk) (Telegram.runApp tel)
