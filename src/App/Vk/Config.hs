{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Vk.Config
  ( Config (..)
  , Group (..)
  , Token (..)
  , VkReader
  ) where

-- IMPORTS -----------------------------------------------------------------

import Infrastructure.Has

import Control.Monad.Reader ( MonadReader )
import Data.Aeson           ( FromJSON (..), (.:), withObject )
import Data.Text.Extended   ( Text )

-- TYPES AND INSTANCES -----------------------------------------------------

type VkReader r m = (Has Token r, Has Group r, MonadReader r m)

newtype Token = Token { unToken :: Text }
newtype Group = Group { unGroup :: Integer }

data Config = Config
  { cToken :: Token
  , cGroup :: Group
  }

instance FromJSON Config where
  parseJSON = withObject "App.Vk.Config" $ \o -> Config
    <$> (Token <$> o .: "token")
    <*> (Group <$> o .: "group_id")
