{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Telegram.Config
  ( Config (..)
  , Token (..)
  , TelegramReader
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has

import Control.Monad.Reader ( MonadReader )
import Data.Aeson           ( FromJSON (..), (.:), withObject )
import Data.Text.Extended   ( Text )

-- TYPES AND INSTANCES -----------------------------------------------------

type TelegramReader r m = (Has Token r, MonadReader r m)

newtype Token = Token { unToken :: Text }

newtype Config = Config { cToken :: Token }

instance FromJSON Config where
  parseJSON = withObject "App.Vk.Config" $ \o -> Config
    <$> (Token <$> o .: "token")
