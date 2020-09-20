{-# LANGUAGE DeriveGeneric     #-}

module Config ( Config (..) ) where

import qualified App.Shared.Config     as Shared
import qualified App.Telegram          as Telegram
import qualified App.Vk.Config         as Vk
import qualified Infrastructure.Logger as Logger

import Data.Aeson.Extended ( FromJSON (..), parseJsonDrop )
import GHC.Generics        ( Generic )

data Config = Config
  { cLogger   :: Logger.Config
  , cVk       :: Vk.Config
  , cTelegram :: Telegram.Config
  , cShared   :: Shared.Config
  } deriving Generic

instance FromJSON Config where
  parseJSON = parseJsonDrop
