{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}

module App ( run ) where

import qualified App.Telegram             as Telegram
import qualified App.Vk                   as Vk
import qualified Infrastructure.Logger    as Logger
import qualified Infrastructure.Requester as Requester

import Control.Concurrent.Async ( concurrently )
import Control.Concurrent.MVar  ( newMVar )
import GHC.Generics             ( Generic )
import Network.HTTP.Client.TLS  ( newTlsManager )

import qualified Data.Aeson.Extended as Aeson

data Config = Config
  { cLogger   :: Logger.Config
  , cVk       :: Vk.Config
  , cTelegram :: Telegram.Config
  } deriving (Generic)

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
      let vk  = Vk.mkApp       cVk       cLogger lock manager
          tel = Telegram.mkApp cTelegram cLogger lock manager
      concurrently (Vk.runApp vk) (Telegram.runApp tel)
      return ()
