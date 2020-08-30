{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards           #-}

module App ( run ) where

import qualified App.Telegram             as Telegram
import qualified App.Vk                   as Vk
import qualified Infrastructure.Logger    as Logger
import qualified Infrastructure.Requester as Requester

import Control.Concurrent.Async ( concurrently )
import Control.Concurrent.MVar  ( newMVar )
import Data.Aeson               ( (.:) )

import qualified Data.Aeson as Aeson

data Config = Config
  { cLogger   :: Logger.Config
  , cVk       :: Vk.Config
  , cTelegram :: Telegram.Config
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "EchoBot.Config" $ \o -> Config
    <$> o .: "logger"
    <*> o .: "vk"
    <*> o .: "telegram"

run :: IO ()
run = do
  lock <- newMVar ()
  config <- Aeson.eitherDecodeFileStrict "bot.conf.local"
  case (config :: Either String Config) of
    Left l -> putStrLn l
    Right Config {..} -> do
      let vk  = Vk.mkApp       cVk       cLogger lock
          tel = Telegram.mkApp cTelegram cLogger lock
      concurrently (Vk.runApp vk) (Telegram.runApp tel)
      return ()
